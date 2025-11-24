//! std.log is a standardized interface for logging which allows for the logging
//! of programs and libraries using this interface to be formatted and filtered
//! by the implementer of the `std.options.logFn` function.
//!
//! Each log message has an associated scope enum, which can be used to give
//! context to the logging. The logging functions in std.log implicitly use a
//! scope of .default.
//!
//! A logging namespace using a custom scope can be created using the
//! std.log.scoped function, passing the scope as an argument; the logging
//! functions in the resulting struct use the provided scope parameter.
//! For example, a library called 'libfoo' might use
//! `const log = std.log.scoped(.libfoo);` to use .libfoo as the scope of its
//! log messages.
//!
//! For an example implementation of the `logFn` function, see `defaultLog`,
//! which is the default implementation. It outputs to stderr, using color if
//! the detected `std.Io.tty.Config` supports it. Its output looks like this:
//! ```
//! error: this is an error
//! error(scope): this is an error with a non-default scope
//! warning: this is a warning
//! info: this is an informative message
//! debug: this is a debugging message
//! ```

const std = @import("std.zig");
const builtin = @import("builtin");

pub const Level = enum {
    /// Error: something has gone wrong. This might be recoverable or might
    /// be followed by the program exiting.
    err,
    /// Warning: it is uncertain if something has gone wrong or not, but the
    /// circumstances would be worth investigating.
    warn,
    /// Info: general messages about the state of the program.
    info,
    /// Debug: messages only useful for debugging.
    debug,

    /// Returns a string literal of the given level in full text form.
    pub fn asText(comptime self: Level) []const u8 {
        return switch (self) {
            .err => "error",
            .warn => "warning",
            .info => "info",
            .debug => "debug",
        };
    }
};

/// The default log level is based on build mode.
pub const default_level: Level = switch (builtin.mode) {
    .Debug => .debug,
    .ReleaseSafe, .ReleaseFast, .ReleaseSmall => .info,
};

pub const ScopeLevel = struct {
    scope: @EnumLiteral(),
    level: Level,
};

fn log(
    comptime level: Level,
    comptime scope: @EnumLiteral(),
    comptime format: []const u8,
    args: anytype,
) void {
    if (comptime !logEnabled(level, scope)) return;

    std.options.logFn(level, scope, format, args);
}

/// Determine if a specific log message level and scope combination are enabled for logging.
pub fn logEnabled(comptime level: Level, comptime scope: @EnumLiteral()) bool {
    inline for (std.options.log_scope_levels) |scope_level| {
        if (scope_level.scope == scope) return @intFromEnum(level) <= @intFromEnum(scope_level.level);
    }
    return @intFromEnum(level) <= @intFromEnum(std.options.log_level);
}

/// The default implementation for the log function. Custom log functions may
/// forward log messages to this function.
///
/// Uses a 64-byte buffer for formatted printing which is flushed before this
/// function returns.
pub fn defaultLog(
    comptime level: Level,
    comptime scope: @EnumLiteral(),
    comptime format: []const u8,
    args: anytype,
) void {
    var buffer: [64]u8 = undefined;
    const stderr, const ttyconf = std.debug.lockStderrWriter(&buffer);
    defer std.debug.unlockStderrWriter();
    ttyconf.setColor(stderr, switch (level) {
        .err => .red,
        .warn => .yellow,
        .info => .green,
        .debug => .magenta,
    }) catch {};
    ttyconf.setColor(stderr, .bold) catch {};
    stderr.writeAll(level.asText()) catch return;
    ttyconf.setColor(stderr, .reset) catch {};
    ttyconf.setColor(stderr, .dim) catch {};
    ttyconf.setColor(stderr, .bold) catch {};
    if (scope != .default) {
        stderr.print("({s})", .{@tagName(scope)}) catch return;
    }
    stderr.writeAll(": ") catch return;
    ttyconf.setColor(stderr, .reset) catch {};
    stderr.print(format ++ "\n", args) catch return;
}

fn trace(
    comptime level: Level,
    comptime scope: @EnumLiteral(),
    any_span: *AnySpan,
    event: SpanEvent,
    executor: ExecutorId,
    comptime format: []const u8,
    args: anytype,
) void {
    if (comptime !logEnabled(level, scope)) return;

    std.options.traceFn(level, scope, any_span, event, executor, format, args);
}

pub fn defaultTrace(
    comptime level: Level,
    comptime scope: @EnumLiteral(),
    any_span: *AnySpan,
    event: SpanEvent,
    executor: ExecutorId,
    comptime format: []const u8,
    args: anytype,
) void {
    _ = any_span;
    _ = executor;
    switch (event) {
        .begin => log(level, scope, "[>>] " ++ format, args),
        .end => log(level, scope, "[<<] " ++ format, args),
        else => {},
    }
}

/// This thread's current executor ID.
pub threadlocal var current_executor: ExecutorId = .none;

/// This thread's currently tracked span.
pub threadlocal var current_span: ?*AnySpan = null;

/// A monotonically increasing, unique identifier for an "executor". Depending
/// on the `std.Io` implementation in use, an executor can be a native thread, a
/// green thread, etc.
pub const ExecutorId = enum(u64) {
    none = std.math.maxInt(u64),
    _,

    /// A globally unique, monotonically increasing executor identifier.
    var next_id: std.atomic.Value(u64) = .init(0);

    pub fn create() ExecutorId {
        return @enumFromInt(next_id.fetchAdd(1, .monotonic));
    }

    pub fn createAndEnter() ExecutorId {
        const self = create();
        self.enter();
        return self;
    }

    /// Acquires a new executor ID, replacing the current one on this thread.
    pub fn enter(self: ExecutorId) void {
        std.debug.assert(current_executor == .none);
        current_executor = self;
    }

    /// Places this executor as the current executor on this thread, asserting
    /// that the current executor is what we expect.
    pub fn compareExchange(self: ExecutorId, expected: ExecutorId) void {
        std.debug.assert(current_executor == expected);
        current_executor = self;
    }

    /// Releases this executor ID, setting the current executor ID to `.none`.
    pub fn exit(self: ExecutorId) void {
        std.debug.assert(current_executor == self);
        current_executor = .none;
    }
};

/// A monotonically increasing, unique identifier for an individual `Span`. Only
/// unique within the application's lifetime.
pub const SpanId = enum(u64) {
    none = std.math.maxInt(u64),
    _,

    /// A globally unique, monotonically increasing `Span` identifier.
    var next_id: std.atomic.Value(u64) = .init(0);

    /// Acquires a new `SpanId`.
    pub fn createNext() SpanId {
        return @enumFromInt(next_id.fetchAdd(1, .monotonic));
    }
};

/// A type erased interface to a `Span`.
pub const AnySpan = struct {
    linkFn: *const fn (*AnySpan) void,
    unlinkFn: *const fn (*AnySpan) void,

    pub const empty: AnySpan = .{
        .linkFn = struct {
            fn link(_: *AnySpan) void {}
        }.link,
        .unlinkFn = struct {
            fn unlink(_: *AnySpan) void {}
        }.unlink,
    };

    pub fn link(self: *AnySpan) void {
        self.linkFn(self);
    }

    pub fn unlink(self: *AnySpan) void {
        self.unlinkFn(self);
    }

    /// Get a pointer to the typed `Span`.
    pub fn asSpan(
        any: *AnySpan,
        comptime level: Level,
        comptime scope: @EnumLiteral(),
        comptime format: []const u8,
        comptime Args: type,
    ) *Span(level, scope, format, Args) {
        const self: *Span(level, scope, format, Args) = @fieldParentPtr("any", any);
        return self;
    }
};

/// A tracing span that is generic over the log level and scope, which can be
/// configured via `std.Options`. When the scope or level has been disabled, this
/// type becomes zero sized and all methods become no-ops, allowing tracing to be
/// compiled out.
///
/// ```zig
/// const span = log.span(.info, "begin request", .{});
/// span.begin();
/// defer span.end();
/// ```
///
/// An initialized span can be moved where it needs to be, but once it has begun,
/// moving or copying a span is illegal behavior.
///
/// When dealing with concurrent execution, to properly track spans in the task
/// that is running concurrently, the original span must be linked and unlinked to
/// the new executor.
///
/// ```zig
/// std.thread.Spawn(.{}, struct {
///     fn myFn(span: *AnySpan) void {
///        span.link();
///        defer span.unlink();
///
///       // new spans on this thread are linked to the original
///     }
/// }.myFn, .{ &span.any });
/// ```
pub fn Span(
    comptime level: Level,
    comptime scope: @EnumLiteral(),
    comptime format: []const u8,
    comptime Args: type,
) type {
    return if (!logEnabled(level, scope)) struct {
        const Self = @This();

        id: void,
        address: void,
        args: void,
        prev: void,
        any: AnySpan = .empty,
        userdata: void,

        pub fn init(_: usize, _: Args) Self {
            return .{};
        }
        pub fn begin(_: *Self) void {}
        pub fn end(_: *Self) void {}
        pub fn enter(_: *Self) void {}
        pub fn exit(_: *Self) void {}
        pub fn link(_: *Self) void {}
        pub fn unlink(_: *Self) void {}
    } else struct {
        const Self = @This();

        id: SpanId,
        address: usize,
        args: Args,
        /// A pointer to the parent span on this executor, forming a linked list.
        prev: ?*AnySpan,
        /// The `AnySpan` type erased interface.
        any: AnySpan,
        /// Custom userdata for the span, defined on `std.Options`.
        userdata: std.options.SpanUserdata,

        /// Initializes the `Span`, but does not begin it.
        pub fn init(address: usize, args: Args) Self {
            return .{
                .id = .createNext(),
                .address = address,
                .args = args,
                .prev = null,
                .any = .{
                    .linkFn = struct {
                        fn link(any: *AnySpan) void {
                            const self = any.asSpan(level, scope, format, Args);
                            return self.link();
                        }
                    }.link,
                    .unlinkFn = struct {
                        fn unlink(any: *AnySpan) void {
                            const self = any.asSpan(level, scope, format, Args);
                            return self.unlink();
                        }
                    }.unlink,
                },
                .userdata = undefined,
            };
        }

        /// Begins the span on this thread's current executor.
        pub fn begin(self: *Self) void {
            trace(level, scope, &self.any, .begin, current_executor, format, self.args);
            self.prev = current_span;
            current_span = &self.any;
        }

        /// Ends the span on this thread's current executor.
        pub fn end(self: *Self) void {
            std.debug.assert(current_span == &self.any);
            trace(level, scope, &self.any, .end, current_executor, format, self.args);
            current_span = self.prev;
            self.* = undefined;
        }

        /// Signals that this span has entered execution on the current executor. Replaces this thread's
        /// current span with itself.
        pub fn enter(self: *Self) void {
            std.debug.assert(current_span == null);
            current_span = &self.any;
            trace(level, scope, &self.any, .enter, current_executor, format, self.args);
        }

        /// Signals that this span has exited execution on the current executor. Replaces this thread's
        /// current span with the previous span.
        pub fn exit(self: *Self) void {
            std.debug.assert(current_span == &self.any);
            trace(level, scope, &self.any, .exit, current_executor, format, self.args);
            current_span = null;
        }

        /// Links the span to this thread's current executor.
        pub fn link(self: *Self) void {
            std.debug.assert(current_span == null);
            trace(level, scope, &self.any, .link, current_executor, format, self.args);
        }

        /// Unlinks the span from this thread's current executor.
        pub fn unlink(self: *Self) void {
            std.debug.assert(current_span == null);
            trace(level, scope, &self.any, .unlink, current_executor, format, self.args);
        }
    };
}

pub const SpanEvent = enum {
    /// The span has begun work on this thread's current executor.
    begin,
    /// The span has ended work on this thread's current executor.
    end,
    /// A child executor has been linked to the span.
    link,
    /// A child executor has been unlinked from the span.
    unlink,
    /// The span itself has moved to an executor.
    enter,
    /// The span itself has moved from an executor.
    exit,
};

/// Returns a scoped logging namespace that logs all messages using the scope
/// provided here.
pub fn scoped(comptime scope: @EnumLiteral()) type {
    return struct {
        /// Log an error message. This log level is intended to be used
        /// when something has gone wrong. This might be recoverable or might
        /// be followed by the program exiting.
        pub fn err(
            comptime format: []const u8,
            args: anytype,
        ) void {
            @branchHint(.cold);
            log(.err, scope, format, args);
        }

        /// Log a warning message. This log level is intended to be used if
        /// it is uncertain whether something has gone wrong or not, but the
        /// circumstances would be worth investigating.
        pub fn warn(
            comptime format: []const u8,
            args: anytype,
        ) void {
            log(.warn, scope, format, args);
        }

        /// Log an info message. This log level is intended to be used for
        /// general messages about the state of the program.
        pub fn info(
            comptime format: []const u8,
            args: anytype,
        ) void {
            log(.info, scope, format, args);
        }

        /// Log a debug message. This log level is intended to be used for
        /// messages which are only useful for debugging.
        pub fn debug(
            comptime format: []const u8,
            args: anytype,
        ) void {
            log(.debug, scope, format, args);
        }

        /// Initialize a new tracing span. The span must be explicitly begun
        /// and ended after initialization.
        pub fn span(
            comptime level: Level,
            comptime format: []const u8,
            args: anytype,
        ) Span(level, scope, format, @TypeOf(args)) {
            return .init(@returnAddress(), args);
        }
    };
}

pub const default_log_scope = .default;

/// The default scoped logging namespace.
pub const default = scoped(default_log_scope);

/// Log an error message using the default scope. This log level is intended to
/// be used when something has gone wrong. This might be recoverable or might
/// be followed by the program exiting.
pub const err = default.err;

/// Log a warning message using the default scope. This log level is intended
/// to be used if it is uncertain whether something has gone wrong or not, but
/// the circumstances would be worth investigating.
pub const warn = default.warn;

/// Log an info message using the default scope. This log level is intended to
/// be used for general messages about the state of the program.
pub const info = default.info;

/// Log a debug message using the default scope. This log level is intended to
/// be used for messages which are only useful for debugging.
pub const debug = default.debug;

/// Initialize a new tracing span using the default scope. The span must be
/// explicitly begun and ended after initialization.
pub const span = default.span;
