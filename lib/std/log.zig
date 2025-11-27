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
const assert = std.debug.assert;
const builtin = @import("builtin");
const SourceLocation = std.builtin.SourceLocation;
const SpanUserdata = std.options.SpanUserdata;

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
    comptime level: log.Level,
    comptime scope: @EnumLiteral(),
    comptime src: SourceLocation,
    comptime event: log.SpanEvent,
    executor: log.Executor,
    span: *log.Span,
) void {
    if (comptime !logEnabled(level, scope)) return;
    std.options.traceFn(level, scope, event, executor, span);
}

pub fn defaultTrace(
    comptime level: log.Level,
    comptime scope: @EnumLiteral(),
    comptime src: SourceLocation,
    comptime event: log.SpanEvent,
    executor: log.Executor,
    span: *log.Span,
) void {
    _ = level;
    _ = scope;
    _ = event;
    _ = executor;
    _ = span;
}

/// This thread's currently running executor.
pub threadlocal var thread_executor: Executor = .none;

/// This thread's currently excuting span.
pub threadlocal var thread_span: Span = .empty;

/// An executor can be a thread, fiber, or whatever the std.Io implementation
/// decides it is. Internally it is represented by a monotonically increasing
/// integer, but that is an implementation detail, and should not be relied
/// upon.
pub const Executor = enum(u64) {
    none = std.math.maxInt(u64),
    _,

    /// A globally unique, monotonically increasing executor identifier.
    var next_id: std.atomic.Value(u64) = .init(0);

    pub fn create() Executor {
        return @enumFromInt(next_id.fetchAdd(1, .monotonic));
    }

    pub fn link(self: Executor, span_: *Span) void {
        if (span_.id == .none) return;
        span_.vtable.linkFn(span_, self);
    }

    pub fn unlink(self: Executor, span_: *Span) void {
        if (span_.id == .none) return;
        span_.vtable.unlinkFn(span_, self);
    }
};

/// An execution span.
pub const Span = struct {
    id: SpanId,
    vtable: *const VTable,
    userdata: SpanUserdata,

    pub const empty: Span = .{
        .id = .none,
        .vtable = undefined,
        .userdata = undefined,
    };

    pub const VTable = struct {
        suspendFn: *const fn (self: *Span) void,
        resumeFn: *const fn (self: *Span) void,
        linkFn: *const fn (self: *Span, executor: Executor) void,
        unlinkFn: *const fn (self: *Span, executor: Executor) void,
    };

    pub fn @"suspend"(self: *Span) Span {
        if (self.id == .none) return empty;
        return self.vtable.suspendFn(self);
    }

    pub fn @"resume"(self: *Span) void {
        if (self.id == .none) return;
        self.vtable.resumeFn(self);
    }
};

/// Internally this is represented by a monotonically increasing integer, but
/// that is an implementation detail, and should not be relied upon.
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
pub fn ScopedSpan(comptime level: Level, comptime scope: @EnumLiteral(), comptime src: std.builtin.SourceLocation) type {
    return if (!logEnabled(level, scope)) struct {
        const Self = @This();

        pub fn begin() Self {
            return .{};
        }
        pub fn end(_: *Self) void {}
    } else struct {
        const Self = @This();

        id: SpanId,
        prev: Span,

        pub fn begin() Self {
            const id: SpanId = .createNext();
            const prev = thread_span;
            thread_span = .{
                .id = id,
                .vtable = &.{
                    .suspendFn = @"suspend",
                    .resumeFn = @"resume",
                    .linkFn = link,
                    .unlinkFn = unlink,
                },
                .userdata = undefined,
            };
            trace(level, scope, src, .begin, thread_executor, &thread_span);
            return .{ .id = id, .prev = prev };
        }

        pub fn end(self: *Self) void {
            // Swap the previous span, that we stored from begin,
            assert(thread_span.id != .none);
            assert(thread_span.id == self.id);
            trace(level, scope, src, .end, thread_executor, &thread_span);
            thread_span = self.span;
            self.* = undefined;
        }

        fn @"suspend"(span_: *Span) Span {
            assert(span_.id != .none);
            assert(thread_span.id != .none);
            assert(thread_span.id == span_.id);
            trace(level, scope, src, .@"suspend", thread_executor, &thread_span);
            const suspended = span_.*;
            thread_span = .empty;
            return suspended;
        }

        fn @"resume"(span_: *Span) void {
            assert(span_.id != .none);
            assert(thread_span.id == .none);
            thread_span = span_.*;
            trace(level, scope, src, .@"resume", thread_executor, &thread_span);
        }

        fn link(span_: *Span, executor: Executor) void {
            assert(executor != .none);
            assert(span_.id != .none);
            assert(thread_executor == .none);
            assert(thread_span.id == .none);
            thread_executor = executor;
            trace(level, scope, src, .link, thread_executor, span_);
        }

        fn unlink(span_: *Span, executor: Executor) void {
            assert(thread_executor != .none);
            assert(thread_executor == executor);
            assert(thread_span.id == .none);
            trace(level, scope, src, .unlink, thread_executor, span_);
            thread_executor = null;
        }
    };
}

pub const SpanEvent = enum {
    /// An executor has begun work on this span.
    begin,
    /// An executor has completed work on this span.
    end,
    /// An executor has suspended work on this span.
    @"suspend",
    /// An executor has resumed work on this span.
    @"resume",
    /// An executor has started work requested within the span.
    link,
    /// An executor has stopped work requested within the span.
    unlink,
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
            comptime src: SourceLocation,
        ) ScopedSpan(level, scope, src) {
            return .begin();
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
