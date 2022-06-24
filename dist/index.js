function asyncGeneratorStep(gen, resolve, reject, _next, _throw, key, arg) {
  try {
    var info = gen[key](arg);
    var value = info.value;
  } catch (error) {
    reject(error);
    return;
  }

  if (info.done) {
    resolve(value);
  } else {
    Promise.resolve(value).then(_next, _throw);
  }
}

function _asyncToGenerator(fn) {
  return function () {
    var self = this,
        args = arguments;
    return new Promise(function (resolve, reject) {
      var gen = fn.apply(self, args);

      function _next(value) {
        asyncGeneratorStep(gen, resolve, reject, _next, _throw, "next", value);
      }

      function _throw(err) {
        asyncGeneratorStep(gen, resolve, reject, _next, _throw, "throw", err);
      }

      _next(undefined);
    });
  };
}

function _extends() {
  _extends = Object.assign || function (target) {
    for (var i = 1; i < arguments.length; i++) {
      var source = arguments[i];

      for (var key in source) {
        if (Object.prototype.hasOwnProperty.call(source, key)) {
          target[key] = source[key];
        }
      }
    }

    return target;
  };

  return _extends.apply(this, arguments);
}

function _unsupportedIterableToArray(o, minLen) {
  if (!o) return;
  if (typeof o === "string") return _arrayLikeToArray(o, minLen);
  var n = Object.prototype.toString.call(o).slice(8, -1);
  if (n === "Object" && o.constructor) n = o.constructor.name;
  if (n === "Map" || n === "Set") return Array.from(o);
  if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen);
}

function _arrayLikeToArray(arr, len) {
  if (len == null || len > arr.length) len = arr.length;

  for (var i = 0, arr2 = new Array(len); i < len; i++) arr2[i] = arr[i];

  return arr2;
}

function _createForOfIteratorHelperLoose(o, allowArrayLike) {
  var it = typeof Symbol !== "undefined" && o[Symbol.iterator] || o["@@iterator"];
  if (it) return (it = it.call(o)).next.bind(it);

  if (Array.isArray(o) || (it = _unsupportedIterableToArray(o)) || allowArrayLike && o && typeof o.length === "number") {
    if (it) o = it;
    var i = 0;
    return function () {
      if (i >= o.length) return {
        done: true
      };
      return {
        done: false,
        value: o[i++]
      };
    };
  }

  throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
}

function createCommonjsModule(fn, module) {
	return module = { exports: {} }, fn(module, module.exports), module.exports;
}

var runtime_1 = createCommonjsModule(function (module) {
/**
 * Copyright (c) 2014-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

var runtime = (function (exports) {

  var Op = Object.prototype;
  var hasOwn = Op.hasOwnProperty;
  var undefined$1; // More compressible than void 0.
  var $Symbol = typeof Symbol === "function" ? Symbol : {};
  var iteratorSymbol = $Symbol.iterator || "@@iterator";
  var asyncIteratorSymbol = $Symbol.asyncIterator || "@@asyncIterator";
  var toStringTagSymbol = $Symbol.toStringTag || "@@toStringTag";

  function define(obj, key, value) {
    Object.defineProperty(obj, key, {
      value: value,
      enumerable: true,
      configurable: true,
      writable: true
    });
    return obj[key];
  }
  try {
    // IE 8 has a broken Object.defineProperty that only works on DOM objects.
    define({}, "");
  } catch (err) {
    define = function(obj, key, value) {
      return obj[key] = value;
    };
  }

  function wrap(innerFn, outerFn, self, tryLocsList) {
    // If outerFn provided and outerFn.prototype is a Generator, then outerFn.prototype instanceof Generator.
    var protoGenerator = outerFn && outerFn.prototype instanceof Generator ? outerFn : Generator;
    var generator = Object.create(protoGenerator.prototype);
    var context = new Context(tryLocsList || []);

    // The ._invoke method unifies the implementations of the .next,
    // .throw, and .return methods.
    generator._invoke = makeInvokeMethod(innerFn, self, context);

    return generator;
  }
  exports.wrap = wrap;

  // Try/catch helper to minimize deoptimizations. Returns a completion
  // record like context.tryEntries[i].completion. This interface could
  // have been (and was previously) designed to take a closure to be
  // invoked without arguments, but in all the cases we care about we
  // already have an existing method we want to call, so there's no need
  // to create a new function object. We can even get away with assuming
  // the method takes exactly one argument, since that happens to be true
  // in every case, so we don't have to touch the arguments object. The
  // only additional allocation required is the completion record, which
  // has a stable shape and so hopefully should be cheap to allocate.
  function tryCatch(fn, obj, arg) {
    try {
      return { type: "normal", arg: fn.call(obj, arg) };
    } catch (err) {
      return { type: "throw", arg: err };
    }
  }

  var GenStateSuspendedStart = "suspendedStart";
  var GenStateSuspendedYield = "suspendedYield";
  var GenStateExecuting = "executing";
  var GenStateCompleted = "completed";

  // Returning this object from the innerFn has the same effect as
  // breaking out of the dispatch switch statement.
  var ContinueSentinel = {};

  // Dummy constructor functions that we use as the .constructor and
  // .constructor.prototype properties for functions that return Generator
  // objects. For full spec compliance, you may wish to configure your
  // minifier not to mangle the names of these two functions.
  function Generator() {}
  function GeneratorFunction() {}
  function GeneratorFunctionPrototype() {}

  // This is a polyfill for %IteratorPrototype% for environments that
  // don't natively support it.
  var IteratorPrototype = {};
  define(IteratorPrototype, iteratorSymbol, function () {
    return this;
  });

  var getProto = Object.getPrototypeOf;
  var NativeIteratorPrototype = getProto && getProto(getProto(values([])));
  if (NativeIteratorPrototype &&
      NativeIteratorPrototype !== Op &&
      hasOwn.call(NativeIteratorPrototype, iteratorSymbol)) {
    // This environment has a native %IteratorPrototype%; use it instead
    // of the polyfill.
    IteratorPrototype = NativeIteratorPrototype;
  }

  var Gp = GeneratorFunctionPrototype.prototype =
    Generator.prototype = Object.create(IteratorPrototype);
  GeneratorFunction.prototype = GeneratorFunctionPrototype;
  define(Gp, "constructor", GeneratorFunctionPrototype);
  define(GeneratorFunctionPrototype, "constructor", GeneratorFunction);
  GeneratorFunction.displayName = define(
    GeneratorFunctionPrototype,
    toStringTagSymbol,
    "GeneratorFunction"
  );

  // Helper for defining the .next, .throw, and .return methods of the
  // Iterator interface in terms of a single ._invoke method.
  function defineIteratorMethods(prototype) {
    ["next", "throw", "return"].forEach(function(method) {
      define(prototype, method, function(arg) {
        return this._invoke(method, arg);
      });
    });
  }

  exports.isGeneratorFunction = function(genFun) {
    var ctor = typeof genFun === "function" && genFun.constructor;
    return ctor
      ? ctor === GeneratorFunction ||
        // For the native GeneratorFunction constructor, the best we can
        // do is to check its .name property.
        (ctor.displayName || ctor.name) === "GeneratorFunction"
      : false;
  };

  exports.mark = function(genFun) {
    if (Object.setPrototypeOf) {
      Object.setPrototypeOf(genFun, GeneratorFunctionPrototype);
    } else {
      genFun.__proto__ = GeneratorFunctionPrototype;
      define(genFun, toStringTagSymbol, "GeneratorFunction");
    }
    genFun.prototype = Object.create(Gp);
    return genFun;
  };

  // Within the body of any async function, `await x` is transformed to
  // `yield regeneratorRuntime.awrap(x)`, so that the runtime can test
  // `hasOwn.call(value, "__await")` to determine if the yielded value is
  // meant to be awaited.
  exports.awrap = function(arg) {
    return { __await: arg };
  };

  function AsyncIterator(generator, PromiseImpl) {
    function invoke(method, arg, resolve, reject) {
      var record = tryCatch(generator[method], generator, arg);
      if (record.type === "throw") {
        reject(record.arg);
      } else {
        var result = record.arg;
        var value = result.value;
        if (value &&
            typeof value === "object" &&
            hasOwn.call(value, "__await")) {
          return PromiseImpl.resolve(value.__await).then(function(value) {
            invoke("next", value, resolve, reject);
          }, function(err) {
            invoke("throw", err, resolve, reject);
          });
        }

        return PromiseImpl.resolve(value).then(function(unwrapped) {
          // When a yielded Promise is resolved, its final value becomes
          // the .value of the Promise<{value,done}> result for the
          // current iteration.
          result.value = unwrapped;
          resolve(result);
        }, function(error) {
          // If a rejected Promise was yielded, throw the rejection back
          // into the async generator function so it can be handled there.
          return invoke("throw", error, resolve, reject);
        });
      }
    }

    var previousPromise;

    function enqueue(method, arg) {
      function callInvokeWithMethodAndArg() {
        return new PromiseImpl(function(resolve, reject) {
          invoke(method, arg, resolve, reject);
        });
      }

      return previousPromise =
        // If enqueue has been called before, then we want to wait until
        // all previous Promises have been resolved before calling invoke,
        // so that results are always delivered in the correct order. If
        // enqueue has not been called before, then it is important to
        // call invoke immediately, without waiting on a callback to fire,
        // so that the async generator function has the opportunity to do
        // any necessary setup in a predictable way. This predictability
        // is why the Promise constructor synchronously invokes its
        // executor callback, and why async functions synchronously
        // execute code before the first await. Since we implement simple
        // async functions in terms of async generators, it is especially
        // important to get this right, even though it requires care.
        previousPromise ? previousPromise.then(
          callInvokeWithMethodAndArg,
          // Avoid propagating failures to Promises returned by later
          // invocations of the iterator.
          callInvokeWithMethodAndArg
        ) : callInvokeWithMethodAndArg();
    }

    // Define the unified helper method that is used to implement .next,
    // .throw, and .return (see defineIteratorMethods).
    this._invoke = enqueue;
  }

  defineIteratorMethods(AsyncIterator.prototype);
  define(AsyncIterator.prototype, asyncIteratorSymbol, function () {
    return this;
  });
  exports.AsyncIterator = AsyncIterator;

  // Note that simple async functions are implemented on top of
  // AsyncIterator objects; they just return a Promise for the value of
  // the final result produced by the iterator.
  exports.async = function(innerFn, outerFn, self, tryLocsList, PromiseImpl) {
    if (PromiseImpl === void 0) PromiseImpl = Promise;

    var iter = new AsyncIterator(
      wrap(innerFn, outerFn, self, tryLocsList),
      PromiseImpl
    );

    return exports.isGeneratorFunction(outerFn)
      ? iter // If outerFn is a generator, return the full iterator.
      : iter.next().then(function(result) {
          return result.done ? result.value : iter.next();
        });
  };

  function makeInvokeMethod(innerFn, self, context) {
    var state = GenStateSuspendedStart;

    return function invoke(method, arg) {
      if (state === GenStateExecuting) {
        throw new Error("Generator is already running");
      }

      if (state === GenStateCompleted) {
        if (method === "throw") {
          throw arg;
        }

        // Be forgiving, per 25.3.3.3.3 of the spec:
        // https://people.mozilla.org/~jorendorff/es6-draft.html#sec-generatorresume
        return doneResult();
      }

      context.method = method;
      context.arg = arg;

      while (true) {
        var delegate = context.delegate;
        if (delegate) {
          var delegateResult = maybeInvokeDelegate(delegate, context);
          if (delegateResult) {
            if (delegateResult === ContinueSentinel) continue;
            return delegateResult;
          }
        }

        if (context.method === "next") {
          // Setting context._sent for legacy support of Babel's
          // function.sent implementation.
          context.sent = context._sent = context.arg;

        } else if (context.method === "throw") {
          if (state === GenStateSuspendedStart) {
            state = GenStateCompleted;
            throw context.arg;
          }

          context.dispatchException(context.arg);

        } else if (context.method === "return") {
          context.abrupt("return", context.arg);
        }

        state = GenStateExecuting;

        var record = tryCatch(innerFn, self, context);
        if (record.type === "normal") {
          // If an exception is thrown from innerFn, we leave state ===
          // GenStateExecuting and loop back for another invocation.
          state = context.done
            ? GenStateCompleted
            : GenStateSuspendedYield;

          if (record.arg === ContinueSentinel) {
            continue;
          }

          return {
            value: record.arg,
            done: context.done
          };

        } else if (record.type === "throw") {
          state = GenStateCompleted;
          // Dispatch the exception by looping back around to the
          // context.dispatchException(context.arg) call above.
          context.method = "throw";
          context.arg = record.arg;
        }
      }
    };
  }

  // Call delegate.iterator[context.method](context.arg) and handle the
  // result, either by returning a { value, done } result from the
  // delegate iterator, or by modifying context.method and context.arg,
  // setting context.delegate to null, and returning the ContinueSentinel.
  function maybeInvokeDelegate(delegate, context) {
    var method = delegate.iterator[context.method];
    if (method === undefined$1) {
      // A .throw or .return when the delegate iterator has no .throw
      // method always terminates the yield* loop.
      context.delegate = null;

      if (context.method === "throw") {
        // Note: ["return"] must be used for ES3 parsing compatibility.
        if (delegate.iterator["return"]) {
          // If the delegate iterator has a return method, give it a
          // chance to clean up.
          context.method = "return";
          context.arg = undefined$1;
          maybeInvokeDelegate(delegate, context);

          if (context.method === "throw") {
            // If maybeInvokeDelegate(context) changed context.method from
            // "return" to "throw", let that override the TypeError below.
            return ContinueSentinel;
          }
        }

        context.method = "throw";
        context.arg = new TypeError(
          "The iterator does not provide a 'throw' method");
      }

      return ContinueSentinel;
    }

    var record = tryCatch(method, delegate.iterator, context.arg);

    if (record.type === "throw") {
      context.method = "throw";
      context.arg = record.arg;
      context.delegate = null;
      return ContinueSentinel;
    }

    var info = record.arg;

    if (! info) {
      context.method = "throw";
      context.arg = new TypeError("iterator result is not an object");
      context.delegate = null;
      return ContinueSentinel;
    }

    if (info.done) {
      // Assign the result of the finished delegate to the temporary
      // variable specified by delegate.resultName (see delegateYield).
      context[delegate.resultName] = info.value;

      // Resume execution at the desired location (see delegateYield).
      context.next = delegate.nextLoc;

      // If context.method was "throw" but the delegate handled the
      // exception, let the outer generator proceed normally. If
      // context.method was "next", forget context.arg since it has been
      // "consumed" by the delegate iterator. If context.method was
      // "return", allow the original .return call to continue in the
      // outer generator.
      if (context.method !== "return") {
        context.method = "next";
        context.arg = undefined$1;
      }

    } else {
      // Re-yield the result returned by the delegate method.
      return info;
    }

    // The delegate iterator is finished, so forget it and continue with
    // the outer generator.
    context.delegate = null;
    return ContinueSentinel;
  }

  // Define Generator.prototype.{next,throw,return} in terms of the
  // unified ._invoke helper method.
  defineIteratorMethods(Gp);

  define(Gp, toStringTagSymbol, "Generator");

  // A Generator should always return itself as the iterator object when the
  // @@iterator function is called on it. Some browsers' implementations of the
  // iterator prototype chain incorrectly implement this, causing the Generator
  // object to not be returned from this call. This ensures that doesn't happen.
  // See https://github.com/facebook/regenerator/issues/274 for more details.
  define(Gp, iteratorSymbol, function() {
    return this;
  });

  define(Gp, "toString", function() {
    return "[object Generator]";
  });

  function pushTryEntry(locs) {
    var entry = { tryLoc: locs[0] };

    if (1 in locs) {
      entry.catchLoc = locs[1];
    }

    if (2 in locs) {
      entry.finallyLoc = locs[2];
      entry.afterLoc = locs[3];
    }

    this.tryEntries.push(entry);
  }

  function resetTryEntry(entry) {
    var record = entry.completion || {};
    record.type = "normal";
    delete record.arg;
    entry.completion = record;
  }

  function Context(tryLocsList) {
    // The root entry object (effectively a try statement without a catch
    // or a finally block) gives us a place to store values thrown from
    // locations where there is no enclosing try statement.
    this.tryEntries = [{ tryLoc: "root" }];
    tryLocsList.forEach(pushTryEntry, this);
    this.reset(true);
  }

  exports.keys = function(object) {
    var keys = [];
    for (var key in object) {
      keys.push(key);
    }
    keys.reverse();

    // Rather than returning an object with a next method, we keep
    // things simple and return the next function itself.
    return function next() {
      while (keys.length) {
        var key = keys.pop();
        if (key in object) {
          next.value = key;
          next.done = false;
          return next;
        }
      }

      // To avoid creating an additional object, we just hang the .value
      // and .done properties off the next function object itself. This
      // also ensures that the minifier will not anonymize the function.
      next.done = true;
      return next;
    };
  };

  function values(iterable) {
    if (iterable) {
      var iteratorMethod = iterable[iteratorSymbol];
      if (iteratorMethod) {
        return iteratorMethod.call(iterable);
      }

      if (typeof iterable.next === "function") {
        return iterable;
      }

      if (!isNaN(iterable.length)) {
        var i = -1, next = function next() {
          while (++i < iterable.length) {
            if (hasOwn.call(iterable, i)) {
              next.value = iterable[i];
              next.done = false;
              return next;
            }
          }

          next.value = undefined$1;
          next.done = true;

          return next;
        };

        return next.next = next;
      }
    }

    // Return an iterator with no values.
    return { next: doneResult };
  }
  exports.values = values;

  function doneResult() {
    return { value: undefined$1, done: true };
  }

  Context.prototype = {
    constructor: Context,

    reset: function(skipTempReset) {
      this.prev = 0;
      this.next = 0;
      // Resetting context._sent for legacy support of Babel's
      // function.sent implementation.
      this.sent = this._sent = undefined$1;
      this.done = false;
      this.delegate = null;

      this.method = "next";
      this.arg = undefined$1;

      this.tryEntries.forEach(resetTryEntry);

      if (!skipTempReset) {
        for (var name in this) {
          // Not sure about the optimal order of these conditions:
          if (name.charAt(0) === "t" &&
              hasOwn.call(this, name) &&
              !isNaN(+name.slice(1))) {
            this[name] = undefined$1;
          }
        }
      }
    },

    stop: function() {
      this.done = true;

      var rootEntry = this.tryEntries[0];
      var rootRecord = rootEntry.completion;
      if (rootRecord.type === "throw") {
        throw rootRecord.arg;
      }

      return this.rval;
    },

    dispatchException: function(exception) {
      if (this.done) {
        throw exception;
      }

      var context = this;
      function handle(loc, caught) {
        record.type = "throw";
        record.arg = exception;
        context.next = loc;

        if (caught) {
          // If the dispatched exception was caught by a catch block,
          // then let that catch block handle the exception normally.
          context.method = "next";
          context.arg = undefined$1;
        }

        return !! caught;
      }

      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        var record = entry.completion;

        if (entry.tryLoc === "root") {
          // Exception thrown outside of any try block that could handle
          // it, so set the completion value of the entire function to
          // throw the exception.
          return handle("end");
        }

        if (entry.tryLoc <= this.prev) {
          var hasCatch = hasOwn.call(entry, "catchLoc");
          var hasFinally = hasOwn.call(entry, "finallyLoc");

          if (hasCatch && hasFinally) {
            if (this.prev < entry.catchLoc) {
              return handle(entry.catchLoc, true);
            } else if (this.prev < entry.finallyLoc) {
              return handle(entry.finallyLoc);
            }

          } else if (hasCatch) {
            if (this.prev < entry.catchLoc) {
              return handle(entry.catchLoc, true);
            }

          } else if (hasFinally) {
            if (this.prev < entry.finallyLoc) {
              return handle(entry.finallyLoc);
            }

          } else {
            throw new Error("try statement without catch or finally");
          }
        }
      }
    },

    abrupt: function(type, arg) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc <= this.prev &&
            hasOwn.call(entry, "finallyLoc") &&
            this.prev < entry.finallyLoc) {
          var finallyEntry = entry;
          break;
        }
      }

      if (finallyEntry &&
          (type === "break" ||
           type === "continue") &&
          finallyEntry.tryLoc <= arg &&
          arg <= finallyEntry.finallyLoc) {
        // Ignore the finally entry if control is not jumping to a
        // location outside the try/catch block.
        finallyEntry = null;
      }

      var record = finallyEntry ? finallyEntry.completion : {};
      record.type = type;
      record.arg = arg;

      if (finallyEntry) {
        this.method = "next";
        this.next = finallyEntry.finallyLoc;
        return ContinueSentinel;
      }

      return this.complete(record);
    },

    complete: function(record, afterLoc) {
      if (record.type === "throw") {
        throw record.arg;
      }

      if (record.type === "break" ||
          record.type === "continue") {
        this.next = record.arg;
      } else if (record.type === "return") {
        this.rval = this.arg = record.arg;
        this.method = "return";
        this.next = "end";
      } else if (record.type === "normal" && afterLoc) {
        this.next = afterLoc;
      }

      return ContinueSentinel;
    },

    finish: function(finallyLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.finallyLoc === finallyLoc) {
          this.complete(entry.completion, entry.afterLoc);
          resetTryEntry(entry);
          return ContinueSentinel;
        }
      }
    },

    "catch": function(tryLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc === tryLoc) {
          var record = entry.completion;
          if (record.type === "throw") {
            var thrown = record.arg;
            resetTryEntry(entry);
          }
          return thrown;
        }
      }

      // The context.catch method must only be called with a location
      // argument that corresponds to a known catch block.
      throw new Error("illegal catch attempt");
    },

    delegateYield: function(iterable, resultName, nextLoc) {
      this.delegate = {
        iterator: values(iterable),
        resultName: resultName,
        nextLoc: nextLoc
      };

      if (this.method === "next") {
        // Deliberately forget the last sent value so that we don't
        // accidentally pass it on to the delegate.
        this.arg = undefined$1;
      }

      return ContinueSentinel;
    }
  };

  // Regardless of whether this script is executing as a CommonJS module
  // or not, return the runtime object so that we can declare the variable
  // regeneratorRuntime in the outer scope, which allows this module to be
  // injected easily by `bin/regenerator --include-runtime script.js`.
  return exports;

}(
  // If this script is executing as a CommonJS module, use module.exports
  // as the regeneratorRuntime namespace. Otherwise create a new empty
  // object. Either way, the resulting object will be used to initialize
  // the regeneratorRuntime variable at the top of this file.
   module.exports 
));

try {
  regeneratorRuntime = runtime;
} catch (accidentalStrictMode) {
  // This module should not be running in strict mode, so the above
  // assignment should always work unless something is misconfigured. Just
  // in case runtime.js accidentally runs in strict mode, in modern engines
  // we can explicitly access globalThis. In older engines we can escape
  // strict mode using a global Function call. This could conceivably fail
  // if a Content Security Policy forbids using Function, but in that case
  // the proper solution is to fix the accidental strict mode problem. If
  // you've misconfigured your bundler to force strict mode and applied a
  // CSP to forbid Function, and you're not willing to fix either of those
  // problems, please detail your unique predicament in a GitHub issue.
  if (typeof globalThis === "object") {
    globalThis.regeneratorRuntime = runtime;
  } else {
    Function("r", "regeneratorRuntime = r")(runtime);
  }
}
});

/* eslint-enable */

/** Check if environment is Node.js or SSR like Gatsby or Next.js */
var importNodeOrSSR = /*#__PURE__*/function () {
  var _ref = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee() {
    return runtime_1.wrap(function _callee$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            _context.prev = 0;
            _context.next = 3;
            return import(
            /* webpackIgnore: true */
            '../custom_modules/cardano-multiplatform-lib-nodejs/cardano_multiplatform_lib.js');

          case 3:
            return _context.abrupt("return", _context.sent);

          case 6:
            _context.prev = 6;
            _context.t0 = _context["catch"](0);
            return _context.abrupt("return", null);

          case 9:
          case "end":
            return _context.stop();
        }
      }
    }, _callee, null, [[0, 6]]);
  }));

  return function importNodeOrSSR() {
    return _ref.apply(this, arguments);
  };
}();

var importDeno = /*#__PURE__*/function () {
  var _ref2 = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee2() {
    var pkg;
    return runtime_1.wrap(function _callee2$(_context2) {
      while (1) {
        switch (_context2.prev = _context2.next) {
          case 0:
            _context2.next = 2;
            return import(
            /* webpackIgnore: true */
            '../custom_modules/cardano-multiplatform-lib-web/cardano_multiplatform_lib.js');

          case 2:
            pkg = _context2.sent;
            _context2.t0 = pkg;
            _context2.next = 6;
            return fetch( // Deno.readFile reads from the root file
            '../custom_modules/cardano-multiplatform-lib-web/cardano_multiplatform_lib_bg.wasm');

          case 6:
            _context2.t1 = _context2.sent;
            _context2.next = 9;
            return _context2.t0["default"].call(_context2.t0, _context2.t1);

          case 9:
            return _context2.abrupt("return", pkg);

          case 10:
          case "end":
            return _context2.stop();
        }
      }
    }, _callee2);
  }));

  return function importDeno() {
    return _ref2.apply(this, arguments);
  };
}();

var C = typeof window !== 'undefined' ? typeof window.Deno !== 'undefined' ? await /*#__PURE__*/importDeno() : await /*#__PURE__*/import('../custom_modules/cardano-multiplatform-lib-browser/cardano_multiplatform_lib.js') : await /*#__PURE__*/importNodeOrSSR();

var costModels = function costModels() {
  var costmdls = C.Costmdls["new"](); // add plutus v1

  var costmdlV1 = C.CostModel["new"]();
  plutusV1.forEach(function (cost, index) {
    costmdlV1.set(index, C.Int.new_i32(cost));
  });
  costmdls.insert(C.Language.new_plutus_v1(), costmdlV1); // add plutus v2 TODO

  var costmdlV2 = C.CostModel.new_plutus_v2();
  plutusV2.forEach(function (cost, index) {
    costmdlV2.set(index, C.Int.new_i32(cost));
  });
  costmdls.insert(C.Language.new_plutus_v2(), costmdlV2);
  return costmdls;
}; // babbage

var plutusV1 = [205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475, 4, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 100, 100, 23000, 100, 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32, 497525, 14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662, 4, 2, 245000, 216773, 62, 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436, 32, 43249, 32, 1000, 32, 80556, 1, 57667, 4, 1000, 10, 197145, 156, 1, 197145, 156, 1, 204924, 473, 1, 208896, 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558, 32, 16563, 32, 76511, 32, 196500, 453240, 220, 0, 1, 1, 69522, 11687, 0, 1, 60091, 32, 196500, 453240, 220, 0, 1, 1, 196500, 453240, 220, 0, 1, 1, 806990, 30482, 4, 1927926, 82523, 4, 265318, 0, 4, 0, 85931, 32, 205665, 812, 1, 1, 41182, 32, 212342, 32, 31220, 32, 32696, 32, 43357, 32, 32247, 32, 38314, 32, 9462713, 1021, 10]; // alonzo
// const plutusV1 = [
//   197209,
//   0,
//   1,
//   1,
//   396231,
//   621,
//   0,
//   1,
//   150000,
//   1000,
//   0,
//   1,
//   150000,
//   32,
//   2477736,
//   29175,
//   4,
//   29773,
//   100,
//   29773,
//   100,
//   29773,
//   100,
//   29773,
//   100,
//   29773,
//   100,
//   29773,
//   100,
//   100,
//   100,
//   29773,
//   100,
//   150000,
//   32,
//   150000,
//   32,
//   150000,
//   32,
//   150000,
//   1000,
//   0,
//   1,
//   150000,
//   32,
//   150000,
//   1000,
//   0,
//   8,
//   148000,
//   425507,
//   118,
//   0,
//   1,
//   1,
//   150000,
//   1000,
//   0,
//   8,
//   150000,
//   112536,
//   247,
//   1,
//   150000,
//   10000,
//   1,
//   136542,
//   1326,
//   1,
//   1000,
//   150000,
//   1000,
//   1,
//   150000,
//   32,
//   150000,
//   32,
//   150000,
//   32,
//   1,
//   1,
//   150000,
//   1,
//   150000,
//   4,
//   103599,
//   248,
//   1,
//   103599,
//   248,
//   1,
//   145276,
//   1366,
//   1,
//   179690,
//   497,
//   1,
//   150000,
//   32,
//   150000,
//   32,
//   150000,
//   32,
//   150000,
//   32,
//   150000,
//   32,
//   150000,
//   32,
//   148000,
//   425507,
//   118,
//   0,
//   1,
//   1,
//   61516,
//   11218,
//   0,
//   1,
//   150000,
//   32,
//   148000,
//   425507,
//   118,
//   0,
//   1,
//   1,
//   148000,
//   425507,
//   118,
//   0,
//   1,
//   1,
//   2477736,
//   29175,
//   4,
//   0,
//   82363,
//   4,
//   150000,
//   5000,
//   0,
//   1,
//   150000,
//   32,
//   197209,
//   0,
//   1,
//   1,
//   150000,
//   32,
//   150000,
//   32,
//   150000,
//   32,
//   150000,
//   32,
//   150000,
//   32,
//   150000,
//   32,
//   150000,
//   32,
//   3345831,
//   1,
//   1,
// ];

var plutusV2 = [205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475, 4, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 100, 100, 23000, 100, 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32, 497525, 14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662, 4, 2, 245000, 216773, 62, 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436, 32, 43249, 32, 1000, 32, 80556, 1, 57667, 4, 1000, 10, 197145, 156, 1, 197145, 156, 1, 204924, 473, 1, 208896, 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558, 32, 16563, 32, 76511, 32, 196500, 453240, 220, 0, 1, 1, 69522, 11687, 0, 1, 60091, 32, 196500, 453240, 220, 0, 1, 1, 196500, 453240, 220, 0, 1, 1, 1159724, 392670, 0, 2, 806990, 30482, 4, 1927926, 82523, 4, 265318, 0, 4, 0, 85931, 32, 205665, 812, 1, 1, 41182, 32, 212342, 32, 31220, 32, 32696, 32, 43357, 32, 32247, 32, 38314, 32, 35892428, 10, 9462713, 1021, 10, 38887044, 32947, 10];

var Utils = /*#__PURE__*/function () {
  function Utils(lucid) {
    this.lucid = lucid;
  }

  var _proto = Utils.prototype;

  _proto.validatorToAddress = function validatorToAddress(validator, stakeCredential) {
    var validatorHash = this.validatorToScriptHash(validator);

    if (stakeCredential) {
      return C.BaseAddress["new"](networkToId(this.lucid.network), C.StakeCredential.from_scripthash(C.ScriptHash.from_hex(validatorHash)), stakeCredential.type === 'Key' ? C.StakeCredential.from_keyhash(C.Ed25519KeyHash.from_hex(stakeCredential.hash)) : C.StakeCredential.from_scripthash(C.ScriptHash.from_hex(stakeCredential.hash))).to_address().to_bech32();
    } else {
      return C.EnterpriseAddress["new"](networkToId(this.lucid.network), C.StakeCredential.from_scripthash(C.ScriptHash.from_hex(validatorHash))).to_address().to_bech32();
    }
  };

  _proto.validatorToScriptHash = function validatorToScriptHash(validator) {
    if (validator.type === 'Native') {
      return C.NativeScript.from_bytes(fromHex(validator.script)).hash(C.ScriptHashNamespace.NativeScript).to_hex();
    } else if (validator.type === 'PlutusV1') {
      return C.PlutusScript.from_bytes(fromHex(validator.script)).hash(C.ScriptHashNamespace.PlutusV1).to_hex();
    } else if (validator.type === 'PlutusV2') {
      return C.PlutusScript.from_bytes(fromHex(validator.script)).hash(C.ScriptHashNamespace.PlutusV2).to_hex();
    }

    throw new Error('No variant matched');
  };

  _proto.scriptHashToCredential = function scriptHashToCredential(scriptHash) {
    return {
      type: 'Script',
      hash: scriptHash
    };
  };

  _proto.keyHashToCredential = function keyHashToCredential(keyHash) {
    return {
      type: 'Key',
      hash: keyHash
    };
  };

  _proto.generatePrivateKey = function generatePrivateKey() {
    return C.PrivateKey.generate_ed25519().to_bech32();
  };

  _proto.unixTimeToSlot = function unixTimeToSlot(unixTime) {
    return this.lucid.network === 'Mainnet' ? _unixTimeToSlot(unixTime) : unixTimeToSlotTestnet(unixTime);
  };

  _proto.slotToUnixTime = function slotToUnixTime(slot) {
    return this.lucid.network === 'Mainnet' ? _slotToUnixTime(slot) : slotToUnixTimeTestnet(slot);
  }
  /** Address can be in bech32 or hex */
  ;

  _proto.getAddressDetails = function getAddressDetails(address) {
    /* eslint no-empty: ["error", { "allowEmptyCatch": true }] */
    // Base Address
    try {
      var parsedAddress = C.BaseAddress.from_address(C.Address.from_bytes(fromHex(address)));
      var paymentCredential = parsedAddress.payment_cred().kind() === 0 ? {
        type: 'Key',
        hash: toHex(parsedAddress.payment_cred().to_keyhash().to_bytes())
      } : {
        type: 'Script',
        hash: toHex(parsedAddress.payment_cred().to_scripthash().to_bytes())
      };
      var stakeCredential = parsedAddress.stake_cred().kind() === 0 ? {
        type: 'Key',
        hash: toHex(parsedAddress.stake_cred().to_keyhash().to_bytes())
      } : {
        type: 'Script',
        hash: toHex(parsedAddress.stake_cred().to_scripthash().to_bytes())
      };
      return {
        address: {
          type: 'Base',
          address: parsedAddress.to_address().to_bech32()
        },
        paymentCredential: paymentCredential,
        stakeCredential: stakeCredential
      };
    } catch (e) {}

    try {
      var _parsedAddress = C.BaseAddress.from_address(C.Address.from_bech32(address));

      var _paymentCredential = _parsedAddress.payment_cred().kind() === 0 ? {
        type: 'Key',
        hash: toHex(_parsedAddress.payment_cred().to_keyhash().to_bytes())
      } : {
        type: 'Script',
        hash: toHex(_parsedAddress.payment_cred().to_scripthash().to_bytes())
      };

      var _stakeCredential = _parsedAddress.stake_cred().kind() === 0 ? {
        type: 'Key',
        hash: toHex(_parsedAddress.stake_cred().to_keyhash().to_bytes())
      } : {
        type: 'Script',
        hash: toHex(_parsedAddress.stake_cred().to_scripthash().to_bytes())
      };

      return {
        address: {
          type: 'Base',
          address: _parsedAddress.to_address().to_bech32()
        },
        paymentCredential: _paymentCredential,
        stakeCredential: _stakeCredential
      };
    } catch (e) {} // Enterprise Address


    try {
      var _parsedAddress2 = C.EnterpriseAddress.from_address(C.Address.from_bytes(fromHex(address)));

      var _paymentCredential2 = _parsedAddress2.payment_cred().kind() === 0 ? {
        type: 'Key',
        hash: toHex(_parsedAddress2.payment_cred().to_keyhash().to_bytes())
      } : {
        type: 'Script',
        hash: toHex(_parsedAddress2.payment_cred().to_scripthash().to_bytes())
      };

      return {
        address: {
          type: 'Enterprise',
          address: _parsedAddress2.to_address().to_bech32()
        },
        paymentCredential: _paymentCredential2
      };
    } catch (e) {}

    try {
      var _parsedAddress3 = C.EnterpriseAddress.from_address(C.Address.from_bech32(address));

      var _paymentCredential3 = _parsedAddress3.payment_cred().kind() === 0 ? {
        type: 'Key',
        hash: toHex(_parsedAddress3.payment_cred().to_keyhash().to_bytes())
      } : {
        type: 'Script',
        hash: toHex(_parsedAddress3.payment_cred().to_scripthash().to_bytes())
      };

      return {
        address: {
          type: 'Enterprise',
          address: _parsedAddress3.to_address().to_bech32()
        },
        paymentCredential: _paymentCredential3
      };
    } catch (e) {} // Pointer Address


    try {
      var _parsedAddress4 = C.PointerAddress.from_address(C.Address.from_bytes(fromHex(address)));

      var _paymentCredential4 = _parsedAddress4.payment_cred().kind() === 0 ? {
        type: 'Key',
        hash: toHex(_parsedAddress4.payment_cred().to_keyhash().to_bytes())
      } : {
        type: 'Script',
        hash: toHex(_parsedAddress4.payment_cred().to_scripthash().to_bytes())
      };

      return {
        address: {
          type: 'Pointer',
          address: _parsedAddress4.to_address().to_bech32()
        },
        paymentCredential: _paymentCredential4
      };
    } catch (e) {}

    try {
      var _parsedAddress5 = C.PointerAddress.from_address(C.Address.from_bech32(address));

      var _paymentCredential5 = _parsedAddress5.payment_cred().kind() === 0 ? {
        type: 'Key',
        hash: toHex(_parsedAddress5.payment_cred().to_keyhash().to_bytes())
      } : {
        type: 'Script',
        hash: toHex(_parsedAddress5.payment_cred().to_scripthash().to_bytes())
      };

      return {
        address: {
          type: 'Pointer',
          address: _parsedAddress5.to_address().to_bech32()
        },
        paymentCredential: _paymentCredential5
      };
    } catch (e) {} // Reward Address


    try {
      var _parsedAddress6 = C.RewardAddress.from_address(C.Address.from_bytes(fromHex(address)));

      var _stakeCredential2 = _parsedAddress6.payment_cred().kind() === 0 ? {
        type: 'Key',
        hash: toHex(_parsedAddress6.payment_cred().to_keyhash().to_bytes())
      } : {
        type: 'Script',
        hash: toHex(_parsedAddress6.payment_cred().to_scripthash().to_bytes())
      };

      return {
        address: {
          type: 'Reward',
          address: _parsedAddress6.to_address().to_bech32()
        },
        stakeCredential: _stakeCredential2
      };
    } catch (e) {}

    try {
      var _parsedAddress7 = C.RewardAddress.from_address(C.Address.from_bech32(address));

      var _stakeCredential3 = _parsedAddress7.payment_cred().kind() === 0 ? {
        type: 'Key',
        hash: toHex(_parsedAddress7.payment_cred().to_keyhash().to_bytes())
      } : {
        type: 'Script',
        hash: toHex(_parsedAddress7.payment_cred().to_scripthash().to_bytes())
      };

      return {
        address: {
          type: 'Reward',
          address: _parsedAddress7.to_address().to_bech32()
        },
        stakeCredential: _stakeCredential3
      };
    } catch (e) {}

    throw new Error('No address type matched for: ' + address);
  };

  return Utils;
}();
var valueToAssets = function valueToAssets(value) {
  var assets = {};
  assets['lovelace'] = BigInt(value.coin().to_str());
  var ma = value.multiasset();

  if (ma) {
    var multiAssets = ma.keys();

    for (var j = 0; j < multiAssets.len(); j++) {
      var policy = multiAssets.get(j);
      var policyAssets = ma.get(policy);
      var assetNames = policyAssets.keys();

      for (var k = 0; k < assetNames.len(); k++) {
        var policyAsset = assetNames.get(k);
        var quantity = policyAssets.get(policyAsset);
        var unit = toHex(policy.to_bytes()) + toHex(policyAsset.name());
        assets[unit] = BigInt(quantity.to_str());
      }
    }
  }

  return assets;
};
var assetsToValue = function assetsToValue(assets) {
  var multiAsset = C.MultiAsset["new"]();
  var lovelace = assets['lovelace'];
  var units = Object.keys(assets);
  var policies = Array.from(new Set(units.filter(function (unit) {
    return unit !== 'lovelace';
  }).map(function (unit) {
    return unit.slice(0, 56);
  })));
  policies.forEach(function (policy) {
    var policyUnits = units.filter(function (unit) {
      return unit.slice(0, 56) === policy;
    });
    var assetsValue = C.Assets["new"]();
    policyUnits.forEach(function (unit) {
      assetsValue.insert(C.AssetName["new"](fromHex(unit.slice(56))), C.BigNum.from_str(assets[unit].toString()));
    });
    multiAsset.insert(C.ScriptHash.from_bytes(fromHex(policy)), assetsValue);
  });
  var value = C.Value["new"](C.BigNum.from_str(lovelace ? lovelace.toString() : '0'));
  if (units.length > 1 || !lovelace) value.set_multiasset(multiAsset);
  return value;
};
var utxoToCore = function utxoToCore(utxo) {
  var address = function () {
    try {
      return C.Address.from_bech32(utxo.address);
    } catch (e) {
      return C.ByronAddress.from_base58(utxo.address).to_address();
    }
  }();

  var output = C.TransactionOutput["new"](address, assetsToValue(utxo.assets));

  if (utxo.datumHash) {
    output.set_datum(C.Datum.new_data_hash(C.DataHash.from_bytes(fromHex(utxo.datumHash))));
  } // inline datum


  if (!utxo.datumHash && utxo.datum) {
    output.set_datum(C.Datum.new_data(C.Data["new"](C.PlutusData.from_bytes(fromHex(utxo.datum)))));
  }

  if (utxo.scriptRef) {
    output.set_script_ref(C.ScriptRef.from_bytes(fromHex(utxo.scriptRef)));
  }

  return C.TransactionUnspentOutput["new"](C.TransactionInput["new"](C.TransactionHash.from_bytes(fromHex(utxo.txHash)), C.BigNum.from_str(utxo.outputIndex.toString())), output);
};
var coreToUtxo = function coreToUtxo(coreUtxo) {
  var _coreUtxo$output$addr, _coreUtxo$output, _coreUtxo$output$datu, _coreUtxo$output$datu2, _coreUtxo$output2, _coreUtxo$output2$dat, _coreUtxo$output3;

  return {
    txHash: toHex(coreUtxo.input().transaction_id().to_bytes()),
    outputIndex: parseInt(coreUtxo.input().index().to_str()),
    assets: valueToAssets(coreUtxo.output().amount()),
    address: coreUtxo.output().address().as_byron() ? (_coreUtxo$output$addr = coreUtxo.output().address().as_byron()) == null ? void 0 : _coreUtxo$output$addr.to_base58() : coreUtxo.output().address().to_bech32(),
    datumHash: (_coreUtxo$output = coreUtxo.output()) == null ? void 0 : (_coreUtxo$output$datu = _coreUtxo$output.datum()) == null ? void 0 : (_coreUtxo$output$datu2 = _coreUtxo$output$datu.as_data_hash()) == null ? void 0 : _coreUtxo$output$datu2.to_hex(),
    datum: ((_coreUtxo$output2 = coreUtxo.output()) == null ? void 0 : (_coreUtxo$output2$dat = _coreUtxo$output2.datum()) == null ? void 0 : _coreUtxo$output2$dat.as_data()) && toHex(coreUtxo.output().datum().as_data().to_bytes()),
    scriptRef: ((_coreUtxo$output3 = coreUtxo.output()) == null ? void 0 : _coreUtxo$output3.script_ref()) && toHex(coreUtxo.output().script_ref().to_bytes())
  };
};

var networkToId = function networkToId(network) {
  if (network === 'Testnet') return 0;else if (network === 'Mainnet') return 1;
  throw new Error('Network not found');
};

var fromHex = function fromHex(hex) {
  return Buffer.from(hex, 'hex');
};
var toHex = function toHex(bytes) {
  return Buffer.from(bytes).toString('hex');
};

var _unixTimeToSlot = function _unixTimeToSlot(unixTime) {
  return Math.floor((unixTime - 1596491091000 + 4924800000) / 1000);
};

var unixTimeToSlotTestnet = function unixTimeToSlotTestnet(unixTime) {
  return Math.floor((unixTime - 1564431616000 - 29937600000) / 1000);
};

var _slotToUnixTime = function _slotToUnixTime(slot) {
  return 1596491091000 + (slot * 1000 - 4924800000);
};

var slotToUnixTimeTestnet = function slotToUnixTimeTestnet(slot) {
  return 1564431616000 + slot * 1000 + 29937600000;
};

var Construct = function Construct(index, args) {
  this.index = index;
  this.args = args;
};
var Data = /*#__PURE__*/function () {
  function Data() {}

  Data.to = function to(data) {
    var serialize = function serialize(data) {
      try {
        if (typeof data === 'bigint' || typeof data === 'number' || typeof data === 'string' && !isNaN(parseInt(data)) && data.slice(-1) === 'n') {
          var bigint = typeof data === 'string' ? BigInt(data.slice(0, -1)) : data;
          return C.PlutusData.new_integer(C.BigInt.from_str(bigint.toString()));
        } else if (typeof data === 'string') {
          return C.PlutusData.new_bytes(fromHex(data));
        } else if (data instanceof Construct) {
          var index = data.index,
              args = data.args;
          var plutusList = C.PlutusList["new"]();
          args.forEach(function (arg) {
            return plutusList.add(serialize(arg));
          });
          return C.PlutusData.new_constr_plutus_data(C.ConstrPlutusData["new"](C.BigNum.from_str(index.toString()), plutusList));
        } else if (data instanceof Array) {
          var _plutusList = C.PlutusList["new"]();

          data.forEach(function (arg) {
            return _plutusList.add(serialize(arg));
          });
          return C.PlutusData.new_list(_plutusList);
        } else if (data instanceof Map) {
          var plutusMap = C.PlutusMap["new"]();

          for (var _iterator = _createForOfIteratorHelperLoose(data.entries()), _step; !(_step = _iterator()).done;) {
            var _step$value = _step.value,
                key = _step$value[0],
                value = _step$value[1];
            plutusMap.insert(serialize(key), serialize(value));
          }

          return C.PlutusData.new_map(plutusMap);
        }

        throw new Error('Unsupported type');
      } catch (error) {
        throw new Error('Could not serialize the data: ' + error);
      }
    };

    return toHex(serialize(data).to_bytes());
  };

  Data.from = function from(data) {
    var plutusData = C.PlutusData.from_bytes(fromHex(data));

    var deserialize = function deserialize(data) {
      if (data.kind() === 0) {
        var constr = data.as_constr_plutus_data();
        var l = constr.data();
        var desL = [];

        for (var i = 0; i < l.len(); i++) {
          desL.push(deserialize(l.get(i)));
        }

        return new Construct(parseInt(constr.alternative().to_str()), desL);
      } else if (data.kind() === 1) {
        var m = data.as_map();
        var desM = new Map();
        var keys = m.keys();

        for (var _i = 0; _i < keys.len(); _i++) {
          desM.set(deserialize(keys.get(_i)), deserialize(m.get(keys.get(_i))));
        }

        return desM;
      } else if (data.kind() === 2) {
        var _l = data.as_list();

        var _desL = [];

        for (var _i2 = 0; _i2 < _l.len(); _i2++) {
          _desL.push(deserialize(_l.get(_i2)));
        }

        return _desL;
      } else if (data.kind() === 3) {
        return BigInt(data.as_integer().to_str());
      } else if (data.kind() === 4) {
        return toHex(data.as_bytes());
      }

      throw new Error('Unsupported type');
    };

    return deserialize(plutusData);
  };

  Data.empty = function empty() {
    return toHex(C.PlutusData.new_constr_plutus_data(C.ConstrPlutusData["new"](C.BigNum.from_str('0'), C.PlutusList["new"]())).to_bytes());
  };

  return Data;
}();

var TxSigned = /*#__PURE__*/function () {
  function TxSigned(lucid, tx) {
    this.lucid = lucid;
    this.txSigned = tx;
  }

  var _proto = TxSigned.prototype;

  _proto.submit = /*#__PURE__*/function () {
    var _submit = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee() {
      return runtime_1.wrap(function _callee$(_context) {
        while (1) {
          switch (_context.prev = _context.next) {
            case 0:
              _context.next = 2;
              return this.lucid.wallet.submitTx(this.txSigned);

            case 2:
              return _context.abrupt("return", _context.sent);

            case 3:
            case "end":
              return _context.stop();
          }
        }
      }, _callee, this);
    }));

    function submit() {
      return _submit.apply(this, arguments);
    }

    return submit;
  }();

  return TxSigned;
}();

var TxComplete = /*#__PURE__*/function () {
  function TxComplete(lucid, tx) {
    this.lucid = lucid;
    this.txComplete = tx;
    this.witnessSetBuilder = C.TransactionWitnessSetBuilder["new"]();
    this.tasks = [];
  }

  var _proto = TxComplete.prototype;

  _proto.sign = function sign() {
    var _this = this;

    this.tasks.push( /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee() {
      var witnesses;
      return runtime_1.wrap(function _callee$(_context) {
        while (1) {
          switch (_context.prev = _context.next) {
            case 0:
              _context.next = 2;
              return _this.lucid.wallet.signTx(_this.txComplete);

            case 2:
              witnesses = _context.sent;

              _this.witnessSetBuilder.add_existing(witnesses);

            case 4:
            case "end":
              return _context.stop();
          }
        }
      }, _callee);
    })));
    return this;
  }
  /** Add an extra signature from a private key */
  ;

  _proto.signWithPrivateKey = function signWithPrivateKey(privateKey) {
    var priv = C.PrivateKey.from_bech32(privateKey);
    var witness = C.make_vkey_witness(C.hash_transaction(this.txComplete.body()), priv);
    this.witnessSetBuilder.add_vkey(witness);
    return this;
  }
  /**
   * Signs the transaction and returns the witnesses that were just made
   */
  ;

  _proto.partialSign =
  /*#__PURE__*/
  function () {
    var _partialSign = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee2() {
      var witnesses;
      return runtime_1.wrap(function _callee2$(_context2) {
        while (1) {
          switch (_context2.prev = _context2.next) {
            case 0:
              _context2.next = 2;
              return this.lucid.wallet.signTx(this.txComplete);

            case 2:
              witnesses = _context2.sent;
              this.witnessSetBuilder.add_existing(witnesses);
              return _context2.abrupt("return", toHex(witnesses.to_bytes()));

            case 5:
            case "end":
              return _context2.stop();
          }
        }
      }, _callee2, this);
    }));

    function partialSign() {
      return _partialSign.apply(this, arguments);
    }

    return partialSign;
  }()
  /**
   * Signs the transaction and returns the witnesses that were just made
   *
   * Add an extra signature from a private key */
  ;

  _proto.partialSignWithPrivateKey = function partialSignWithPrivateKey(privateKey) {
    var priv = C.PrivateKey.from_bech32(privateKey);
    var witness = C.make_vkey_witness(C.hash_transaction(this.txComplete.body()), priv);
    this.witnessSetBuilder.add_vkey(witness);
    var witnesses = C.TransactionWitnessSetBuilder["new"]();
    witnesses.add_vkey(witness);
    return toHex(witnesses.build().to_bytes());
  }
  /**
   * Signs the transaction with the given witnesses
   */
  ;

  _proto.assemble = function assemble(witnesses) {
    var _this2 = this;

    witnesses.forEach(function (witness) {
      var witnessParsed = C.TransactionWitnessSet.from_bytes(fromHex(witness));

      _this2.witnessSetBuilder.add_existing(witnessParsed);
    });
    return this;
  };

  _proto.complete = /*#__PURE__*/function () {
    var _complete = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee3() {
      var _iterator, _step, task, signedTx;

      return runtime_1.wrap(function _callee3$(_context3) {
        while (1) {
          switch (_context3.prev = _context3.next) {
            case 0:
              _iterator = _createForOfIteratorHelperLoose(this.tasks);

            case 1:
              if ((_step = _iterator()).done) {
                _context3.next = 7;
                break;
              }

              task = _step.value;
              _context3.next = 5;
              return task();

            case 5:
              _context3.next = 1;
              break;

            case 7:
              this.witnessSetBuilder.add_existing(this.txComplete.witness_set());
              signedTx = C.Transaction["new"](this.txComplete.body(), this.witnessSetBuilder.build(), this.txComplete.auxiliary_data());
              return _context3.abrupt("return", new TxSigned(this.lucid, signedTx));

            case 10:
            case "end":
              return _context3.stop();
          }
        }
      }, _callee3, this);
    }));

    function complete() {
      return _complete.apply(this, arguments);
    }

    return complete;
  }();

  return TxComplete;
}();

var Tx = /*#__PURE__*/function () {
  function Tx(lucid) {
    this.nftMetadata = {};
    this.lucid = lucid;
    this.txBuilder = C.TransactionBuilder["new"](this.lucid.txBuilderConfig);
    this.tasks = [];
    this.nftMetadata = {
      version: 2
    };
  }
  /**
   *
   * Read data from utxos. These utxos are only referenced and not spent
   */


  var _proto = Tx.prototype;

  _proto.readFrom = function readFrom(utxos) {
    var _this = this;

    this.tasks.push( /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee() {
      var _iterator, _step, utxo, plutusData, coreUtxo;

      return runtime_1.wrap(function _callee$(_context) {
        while (1) {
          switch (_context.prev = _context.next) {
            case 0:
              _iterator = _createForOfIteratorHelperLoose(utxos);

            case 1:
              if ((_step = _iterator()).done) {
                _context.next = 13;
                break;
              }

              utxo = _step.value;

              if (!(utxo.datumHash && !utxo.datum)) {
                _context.next = 9;
                break;
              }

              _context.next = 6;
              return _this.lucid.datumOf(utxo);

            case 6:
              utxo.datum = _context.sent;
              // Add datum to witness set, so it can be read from validators
              plutusData = C.PlutusData.from_bytes(fromHex(utxo.datum));

              _this.txBuilder.add_plutus_data(plutusData);

            case 9:
              coreUtxo = utxoToCore(utxo);

              _this.txBuilder.add_reference_input(coreUtxo);

            case 11:
              _context.next = 1;
              break;

            case 13:
            case "end":
              return _context.stop();
          }
        }
      }, _callee);
    })));
    return this;
  }
  /**
   * A public key or native script input
   *
   * With redeemer a plutus script input
   *  */
  ;

  _proto.collectFrom = function collectFrom(utxos, redeemer) {
    var _this2 = this;

    this.tasks.push( /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee2() {
      var _iterator2, _step2, utxo, coreUtxo;

      return runtime_1.wrap(function _callee2$(_context2) {
        while (1) {
          switch (_context2.prev = _context2.next) {
            case 0:
              _iterator2 = _createForOfIteratorHelperLoose(utxos);

            case 1:
              if ((_step2 = _iterator2()).done) {
                _context2.next = 11;
                break;
              }

              utxo = _step2.value;

              if (!(utxo.datumHash && !utxo.datum)) {
                _context2.next = 7;
                break;
              }

              _context2.next = 6;
              return _this2.lucid.datumOf(utxo);

            case 6:
              utxo.datum = _context2.sent;

            case 7:
              coreUtxo = utxoToCore(utxo);

              _this2.txBuilder.add_input(coreUtxo, redeemer && C.ScriptWitness.new_plutus_witness(C.PlutusWitness["new"](C.PlutusData.from_bytes(fromHex(redeemer)), utxo.datumHash && utxo.datum ? C.PlutusData.from_bytes(fromHex(utxo.datum)) : undefined)));

            case 9:
              _context2.next = 1;
              break;

            case 11:
            case "end":
              return _context2.stop();
          }
        }
      }, _callee2);
    })));
    return this;
  }
  /** All assets should be of the same Policy Id.
   *
   * You can chain mintAssets events together if you need to mint assets with different Policy Ids.
   *
   * If the plutus script doesn't need a redeemer, you still neeed to specifiy the empty redeemer.
   *  */
  ;

  _proto.mintAssets = function mintAssets(assets, redeemer) {
    var units = Object.keys(assets);
    var policyId = units[0].slice(0, 56);
    var mintAssets = C.MintAssets["new"]();
    units.forEach(function (unit) {
      if (unit.slice(0, 56) !== policyId) throw new Error('Only one Policy Id allowed. You can chain multiple mintAssets events together if you need to mint assets with different Policy Ids.');
      mintAssets.insert(C.AssetName["new"](fromHex(unit.slice(56))), C.Int.from_str(assets[unit].toString()));
    });
    var scriptHash = C.ScriptHash.from_bytes(fromHex(policyId));
    this.txBuilder.add_mint(scriptHash, mintAssets, redeemer ? C.ScriptWitness.new_plutus_witness(C.PlutusWitness["new"](C.PlutusData.from_bytes(fromHex(redeemer)))) : undefined);
    return this;
  }
  /**
   * Pay to a public key or native script address
   *  */
  ;

  _proto.payToAddress = function payToAddress(address, assets) {
    var output = C.TransactionOutput["new"](C.Address.from_bech32(address), assetsToValue(assets));
    this.txBuilder.add_output(output);
    return this;
  }
  /**
   * Pay to a public key or native script address with datum or scriptRef
   *  */
  ;

  _proto.payToAddressWithData = function payToAddressWithData(address, outputData, assets) {
    if (typeof outputData === 'string') {
      outputData = {
        asHash: outputData
      };
    }

    if (outputData.asHash && outputData.inline) throw new Error('Not allowed to set asHash and inline at the same time.');
    var output = C.TransactionOutput["new"](C.Address.from_bech32(address), assetsToValue(assets));

    if (outputData.asHash) {
      var plutusData = C.PlutusData.from_bytes(fromHex(outputData.asHash));
      output.set_datum(C.Datum.new_data_hash(C.hash_plutus_data(plutusData)));
      this.txBuilder.add_plutus_data(plutusData);
    } else if (outputData.inline) {
      var _plutusData = C.PlutusData.from_bytes(fromHex(outputData.inline));

      output.set_datum(C.Datum.new_data(C.Data["new"](_plutusData)));
    }

    var script = outputData.scriptRef;

    if (script) {
      if (script.type === 'Native') {
        output.set_script_ref(C.ScriptRef["new"](C.Script.new_native(C.NativeScript.from_bytes(fromHex(script.script)))));
      } else if (script.type === 'PlutusV1') {
        output.set_script_ref(C.ScriptRef["new"](C.Script.new_plutus_v1(C.PlutusScript.from_bytes(fromHex(script.script)))));
      } else if (script.type === 'PlutusV2') {
        output.set_script_ref(C.ScriptRef["new"](C.Script.new_plutus_v2(C.PlutusScript.from_bytes(fromHex(script.script)))));
      }
    }

    this.txBuilder.add_output(output);
    return this;
  }
  /**
   * Pay to a plutus script address with datum or scriptRef
   *  */
  ;

  _proto.payToContract = function payToContract(address, outputData, assets) {
    if (typeof outputData === 'string') {
      outputData = {
        asHash: outputData
      };
    }

    if (outputData.asHash && outputData.inline) throw new Error('Not allowed to set asHash and inline at the same time.');

    if (!(outputData.asHash || outputData.inline)) {
      throw new Error('No datum set. Script output becomes unspendable without datum.');
    }

    var output = C.TransactionOutput["new"](C.Address.from_bech32(address), assetsToValue(assets));

    if (outputData.asHash) {
      var plutusData = C.PlutusData.from_bytes(fromHex(outputData.asHash));
      output.set_datum(C.Datum.new_data_hash(C.hash_plutus_data(plutusData)));
      this.txBuilder.add_plutus_data(plutusData);
    } else if (outputData.inline) {
      var _plutusData2 = C.PlutusData.from_bytes(fromHex(outputData.inline));

      output.set_datum(C.Datum.new_data(C.Data["new"](_plutusData2)));
    }

    var script = outputData.scriptRef;

    if (script) {
      if (script.type === 'Native') {
        output.set_script_ref(C.ScriptRef["new"](C.Script.new_native(C.NativeScript.from_bytes(fromHex(script.script)))));
      } else if (script.type === 'PlutusV1') {
        output.set_script_ref(C.ScriptRef["new"](C.Script.new_plutus_v1(C.PlutusScript.from_bytes(fromHex(script.script)))));
      } else if (script.type === 'PlutusV2') {
        output.set_script_ref(C.ScriptRef["new"](C.Script.new_plutus_v2(C.PlutusScript.from_bytes(fromHex(script.script)))));
      }
    }

    this.txBuilder.add_output(output);
    return this;
  }
  /**
   * Delegate to a stake pool
   */
  ;

  _proto.delegateTo = function delegateTo(rewardAddress, poolId, redeemer) {
    var addressDetails = this.lucid.utils.getAddressDetails(rewardAddress);
    if (addressDetails.address.type !== 'Reward' || !addressDetails.stakeCredential) throw new Error('Not a reward address provided.');
    var credential = addressDetails.stakeCredential.type === 'Key' ? C.StakeCredential.from_keyhash(C.Ed25519KeyHash.from_bytes(fromHex(addressDetails.stakeCredential.hash))) : C.StakeCredential.from_scripthash(C.ScriptHash.from_bytes(fromHex(addressDetails.stakeCredential.hash)));
    this.txBuilder.add_certificate(C.Certificate.new_stake_delegation(C.StakeDelegation["new"](credential, C.Ed25519KeyHash.from_bech32(poolId))), redeemer ? C.ScriptWitness.new_plutus_witness(C.PlutusWitness["new"](C.PlutusData.from_bytes(fromHex(redeemer)))) : undefined);
    return this;
  };

  _proto.registerStake = function registerStake(rewardAddress) {
    var addressDetails = this.lucid.utils.getAddressDetails(rewardAddress);
    if (addressDetails.address.type !== 'Reward' || !addressDetails.stakeCredential) throw new Error('Not a reward address provided.');
    var credential = addressDetails.stakeCredential.type === 'Key' ? C.StakeCredential.from_keyhash(C.Ed25519KeyHash.from_bytes(fromHex(addressDetails.stakeCredential.hash))) : C.StakeCredential.from_scripthash(C.ScriptHash.from_bytes(fromHex(addressDetails.stakeCredential.hash)));
    this.txBuilder.add_certificate(C.Certificate.new_stake_registration(C.StakeRegistration["new"](credential)));
    return this;
  };

  _proto.deregisterStake = function deregisterStake(rewardAddress, redeemer) {
    var addressDetails = this.lucid.utils.getAddressDetails(rewardAddress);
    if (addressDetails.address.type !== 'Reward' || !addressDetails.stakeCredential) throw new Error('Not a reward address provided.');
    var credential = addressDetails.stakeCredential.type === 'Key' ? C.StakeCredential.from_keyhash(C.Ed25519KeyHash.from_bytes(fromHex(addressDetails.stakeCredential.hash))) : C.StakeCredential.from_scripthash(C.ScriptHash.from_bytes(fromHex(addressDetails.stakeCredential.hash)));
    this.txBuilder.add_certificate(C.Certificate.new_stake_deregistration(C.StakeDeregistration["new"](credential)), redeemer ? C.ScriptWitness.new_plutus_witness(C.PlutusWitness["new"](C.PlutusData.from_bytes(fromHex(redeemer)))) : undefined);
    return this;
  };

  _proto.withdraw = function withdraw(rewardAddress, amount, redeemer) {
    this.txBuilder.add_withdrawal(C.RewardAddress.from_address(C.Address.from_bech32(rewardAddress)), C.BigNum.from_str(amount.toString()), redeemer ? C.ScriptWitness.new_plutus_witness(C.PlutusWitness["new"](C.PlutusData.from_bytes(fromHex(redeemer)))) : undefined);
    return this;
  }
  /**
   * Needs to be a public key address
   *
   * The PaymentKeyHash is taken when providing a Base, Enterprise or Pointer address
   *
   * The StakeKeyHash is taken when providing a Reward address
   */
  ;

  _proto.addSigner = function addSigner(address) {
    var addressDetails = this.lucid.utils.getAddressDetails(address);
    if (!addressDetails.paymentCredential && !addressDetails.stakeCredential) throw new Error('Not a valid address.');
    var credential = addressDetails.address.type === 'Reward' ? addressDetails.stakeCredential : addressDetails.paymentCredential;
    if (credential.type === 'Script') throw new Error('Only key hashes are allowed as signers.');
    this.txBuilder.add_required_signer(C.Ed25519KeyHash.from_bytes(fromHex(credential.hash)));
    return this;
  };

  _proto.validFrom = function validFrom(unixTime) {
    var slot = this.lucid.utils.unixTimeToSlot(unixTime);
    this.txBuilder.set_validity_start_interval(C.BigNum.from_str(slot.toString()));
    return this;
  };

  _proto.validTo = function validTo(unixTime) {
    var slot = this.lucid.utils.unixTimeToSlot(unixTime);
    this.txBuilder.set_ttl(C.BigNum.from_str(slot.toString()));
    return this;
  };

  _proto.attachMetadata = function attachMetadata(label, metadata) {
    this.txBuilder.add_json_metadatum(C.BigNum.from_str(label.toString()), JSON.stringify(metadata));
    return this;
  }
  /**
   * Converts strings to bytes if prefixed with **'0x'**
   */
  ;

  _proto.attachMetadataWithConversion = function attachMetadataWithConversion(label, metadata) {
    this.txBuilder.add_json_metadatum_with_schema(C.BigNum.from_str(label.toString()), JSON.stringify(metadata), C.MetadataJsonSchema.BasicConversions);
    return this;
  }
  /**
   * Converts strings to bytes if prefixed with **'0x'**
   *
   * Uses version 2 of CIP-0025
   *
   * You don't need to add policy id and asset name to the metadata, only add the details
   */
  ;

  _proto.attachNFTMetadata = function attachNFTMetadata(unit, metadata) {
    var _extends2;

    var policyId = unit.slice(0, 56);
    var assetName = unit.slice(56);
    this.nftMetadata['0x' + policyId] = _extends({}, this.nftMetadata['0x' + policyId] || {}, (_extends2 = {}, _extends2['0x' + assetName] = metadata, _extends2));
    return this;
  };

  _proto.attachSpendingValidator = function attachSpendingValidator(spendingValidator) {
    attachScript(this, spendingValidator);
    return this;
  };

  _proto.attachMintingPolicy = function attachMintingPolicy(mintingPolicy) {
    attachScript(this, mintingPolicy);
    return this;
  };

  _proto.attachCertificateValidator = function attachCertificateValidator(certValidator) {
    attachScript(this, certValidator);
    return this;
  };

  _proto.attachWithdrawalValidator = function attachWithdrawalValidator(withdrawalValidator) {
    attachScript(this, withdrawalValidator);
    return this;
  }
  /**
   * callback cannot be async
   *
   */
  ;

  _proto.applyIf = function applyIf(condition, callback) {
    if (condition) callback(this);
    return this;
  };

  _proto.complete = /*#__PURE__*/function () {
    var _complete = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee3(option) {
      var _option$datum, _option$datum2, _option$datum3, _option$datum4, _option$datum5;

      var _iterator3, _step3, task, utxos, changeAddress;

      return runtime_1.wrap(function _callee3$(_context3) {
        while (1) {
          switch (_context3.prev = _context3.next) {
            case 0:
              if (!(option != null && (_option$datum = option.datum) != null && _option$datum.asHash && option != null && (_option$datum2 = option.datum) != null && _option$datum2.inline)) {
                _context3.next = 2;
                break;
              }

              throw new Error('Not allowed to set asHash and inline at the same time.');

            case 2:
              _iterator3 = _createForOfIteratorHelperLoose(this.tasks);

            case 3:
              if ((_step3 = _iterator3()).done) {
                _context3.next = 9;
                break;
              }

              task = _step3.value;
              _context3.next = 7;
              return task();

            case 7:
              _context3.next = 3;
              break;

            case 9:
              // Add NFT metadata
              if (Object.keys(this.nftMetadata).length > 1) {
                this.txBuilder.add_json_metadatum_with_schema(C.BigNum.from_str('721'), JSON.stringify(this.nftMetadata), C.MetadataJsonSchema.BasicConversions);
              }

              _context3.next = 12;
              return this.lucid.wallet.getUtxosCore();

            case 12:
              utxos = _context3.sent;
              _context3.t0 = C.Address;
              _context3.t1 = option == null ? void 0 : option.changeAddress;

              if (_context3.t1) {
                _context3.next = 19;
                break;
              }

              _context3.next = 18;
              return this.lucid.wallet.address();

            case 18:
              _context3.t1 = _context3.sent;

            case 19:
              _context3.t2 = _context3.t1;
              changeAddress = _context3.t0.from_bech32.call(_context3.t0, _context3.t2);
              this.txBuilder.add_inputs_from(utxos);
              this.txBuilder.balance(changeAddress, option != null && (_option$datum3 = option.datum) != null && _option$datum3.asHash ? C.Datum.new_data_hash(C.hash_plutus_data(C.PlutusData.from_bytes(fromHex(option.datum.asHash)))) : option != null && (_option$datum4 = option.datum) != null && _option$datum4.inline ? C.Datum.new_data(C.Data["new"](C.PlutusData.from_bytes(fromHex(option.datum.inline)))) : undefined);

              if (option != null && (_option$datum5 = option.datum) != null && _option$datum5.asHash) {
                this.txBuilder.add_plutus_data(C.PlutusData.from_bytes(fromHex(option.datum.asHash)));
              }

              _context3.t3 = TxComplete;
              _context3.t4 = this.lucid;
              _context3.next = 28;
              return this.txBuilder.construct(utxos, changeAddress);

            case 28:
              _context3.t5 = _context3.sent;
              return _context3.abrupt("return", new _context3.t3(_context3.t4, _context3.t5));

            case 30:
            case "end":
              return _context3.stop();
          }
        }
      }, _callee3, this);
    }));

    function complete(_x) {
      return _complete.apply(this, arguments);
    }

    return complete;
  }();

  return Tx;
}();

var attachScript = function attachScript(tx, script) {
  if (script.type === 'Native') {
    return tx.txBuilder.add_native_script(C.NativeScript.from_bytes(fromHex(script.script)));
  } else if (script.type === 'PlutusV1') {
    return tx.txBuilder.add_plutus_script(C.PlutusScript.from_bytes(fromHex(script.script)));
  } else if (script.type === 'PlutusV2') {
    return tx.txBuilder.add_plutus_v2_script(C.PlutusScript.from_bytes(fromHex(script.script)));
  }

  throw new Error('No variant matched.');
};

var Lucid = /*#__PURE__*/function () {
  function Lucid() {
    this.network = 'Mainnet';
  }

  Lucid["new"] = /*#__PURE__*/function () {
    var _new2 = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee(provider, network) {
      var lucid, protocolParameters;
      return runtime_1.wrap(function _callee$(_context) {
        while (1) {
          switch (_context.prev = _context.next) {
            case 0:
              lucid = new this();
              if (network) lucid.network = network;

              if (!provider) {
                _context.next = 8;
                break;
              }

              lucid.provider = provider;
              _context.next = 6;
              return provider.getProtocolParameters();

            case 6:
              protocolParameters = _context.sent;
              lucid.txBuilderConfig = C.TransactionBuilderConfigBuilder["new"]().coins_per_utxo_word(C.BigNum.from_str(protocolParameters.coinsPerUtxoByte.toString())).fee_algo(C.LinearFee["new"](C.BigNum.from_str(protocolParameters.minFeeA.toString()), C.BigNum.from_str(protocolParameters.minFeeB.toString()))).key_deposit(C.BigNum.from_str(protocolParameters.keyDeposit.toString())).pool_deposit(C.BigNum.from_str(protocolParameters.poolDeposit.toString())).max_tx_size(protocolParameters.maxTxSize).max_value_size(protocolParameters.maxValSize).collateral_percentage(protocolParameters.collateralPercentage).max_collateral_inputs(protocolParameters.maxCollateralInputs).ex_unit_prices(C.ExUnitPrices.from_float(protocolParameters.priceMem, protocolParameters.priceStep)).blockfrost(C.Blockfrost["new"](provider.url + '/utils/txs/evaluate', provider.projectId)).costmdls(costModels()).build();

            case 8:
              lucid.utils = new Utils(lucid);
              return _context.abrupt("return", lucid);

            case 10:
            case "end":
              return _context.stop();
          }
        }
      }, _callee, this);
    }));

    function _new(_x, _x2) {
      return _new2.apply(this, arguments);
    }

    return _new;
  }();

  var _proto = Lucid.prototype;

  _proto.newTx = function newTx() {
    return new Tx(this);
  };

  _proto.fromTx = function fromTx(tx) {
    return new TxComplete(this, C.Transaction.from_bytes(fromHex(tx)));
  };

  _proto.currentSlot = /*#__PURE__*/function () {
    var _currentSlot = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee2() {
      return runtime_1.wrap(function _callee2$(_context2) {
        while (1) {
          switch (_context2.prev = _context2.next) {
            case 0:
              return _context2.abrupt("return", this.provider.getCurrentSlot());

            case 1:
            case "end":
              return _context2.stop();
          }
        }
      }, _callee2, this);
    }));

    function currentSlot() {
      return _currentSlot.apply(this, arguments);
    }

    return currentSlot;
  }();

  _proto.utxosAt = /*#__PURE__*/function () {
    var _utxosAt = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee3(address) {
      return runtime_1.wrap(function _callee3$(_context3) {
        while (1) {
          switch (_context3.prev = _context3.next) {
            case 0:
              return _context3.abrupt("return", this.provider.getUtxos(address));

            case 1:
            case "end":
              return _context3.stop();
          }
        }
      }, _callee3, this);
    }));

    function utxosAt(_x3) {
      return _utxosAt.apply(this, arguments);
    }

    return utxosAt;
  }();

  _proto.utxosAtWithUnit = /*#__PURE__*/function () {
    var _utxosAtWithUnit = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee4(address, unit) {
      return runtime_1.wrap(function _callee4$(_context4) {
        while (1) {
          switch (_context4.prev = _context4.next) {
            case 0:
              return _context4.abrupt("return", this.provider.getUtxosWithUnit(address, unit));

            case 1:
            case "end":
              return _context4.stop();
          }
        }
      }, _callee4, this);
    }));

    function utxosAtWithUnit(_x4, _x5) {
      return _utxosAtWithUnit.apply(this, arguments);
    }

    return utxosAtWithUnit;
  }();

  _proto.awaitTx = /*#__PURE__*/function () {
    var _awaitTx = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee5(txHash) {
      return runtime_1.wrap(function _callee5$(_context5) {
        while (1) {
          switch (_context5.prev = _context5.next) {
            case 0:
              return _context5.abrupt("return", this.provider.awaitTx(txHash));

            case 1:
            case "end":
              return _context5.stop();
          }
        }
      }, _callee5, this);
    }));

    function awaitTx(_x6) {
      return _awaitTx.apply(this, arguments);
    }

    return awaitTx;
  }();

  _proto.datumOf = /*#__PURE__*/function () {
    var _datumOf = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee6(utxo) {
      return runtime_1.wrap(function _callee6$(_context6) {
        while (1) {
          switch (_context6.prev = _context6.next) {
            case 0:
              if (utxo.datumHash) {
                _context6.next = 2;
                break;
              }

              throw new Error('This UTxO does not have a datum hash.');

            case 2:
              if (!utxo.datum) {
                _context6.next = 4;
                break;
              }

              return _context6.abrupt("return", utxo.datum);

            case 4:
              _context6.next = 6;
              return this.provider.getDatum(utxo.datumHash);

            case 6:
              utxo.datum = _context6.sent;
              return _context6.abrupt("return", utxo.datum);

            case 8:
            case "end":
              return _context6.stop();
          }
        }
      }, _callee6, this);
    }));

    function datumOf(_x7) {
      return _datumOf.apply(this, arguments);
    }

    return datumOf;
  }()
  /**
   * Cardano Private key in bech32; not the BIP32 private key or any key that is not fully derived
   */
  ;

  _proto.selectWalletFromPrivateKey = function selectWalletFromPrivateKey(privateKey) {
    var _this = this;

    var priv = C.PrivateKey.from_bech32(privateKey);
    var pubKeyHash = priv.to_public().hash();
    this.wallet = {
      address: function () {
        var _address = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee7() {
          return runtime_1.wrap(function _callee7$(_context7) {
            while (1) {
              switch (_context7.prev = _context7.next) {
                case 0:
                  return _context7.abrupt("return", C.EnterpriseAddress["new"](_this.network === 'Mainnet' ? 1 : 0, C.StakeCredential.from_keyhash(pubKeyHash)).to_address().to_bech32());

                case 1:
                case "end":
                  return _context7.stop();
              }
            }
          }, _callee7);
        }));

        function address() {
          return _address.apply(this, arguments);
        }

        return address;
      }(),
      rewardAddress: function () {
        var _rewardAddress = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee8() {
          return runtime_1.wrap(function _callee8$(_context8) {
            while (1) {
              switch (_context8.prev = _context8.next) {
                case 0:
                  return _context8.abrupt("return", undefined);

                case 1:
                case "end":
                  return _context8.stop();
              }
            }
          }, _callee8);
        }));

        function rewardAddress() {
          return _rewardAddress.apply(this, arguments);
        }

        return rewardAddress;
      }(),
      getCollateral: function () {
        var _getCollateral = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee9() {
          var utxos;
          return runtime_1.wrap(function _callee9$(_context9) {
            while (1) {
              switch (_context9.prev = _context9.next) {
                case 0:
                  _context9.t0 = _this;
                  _context9.next = 3;
                  return _this.wallet.address();

                case 3:
                  _context9.t1 = _context9.sent;
                  _context9.next = 6;
                  return _context9.t0.utxosAt.call(_context9.t0, _context9.t1);

                case 6:
                  utxos = _context9.sent;
                  return _context9.abrupt("return", utxos.filter(function (utxo) {
                    return Object.keys(utxo.assets).length === 1 && utxo.assets.lovelace >= 5000000n;
                  }));

                case 8:
                case "end":
                  return _context9.stop();
              }
            }
          }, _callee9);
        }));

        function getCollateral() {
          return _getCollateral.apply(this, arguments);
        }

        return getCollateral;
      }(),
      getCollateralCore: function () {
        var _getCollateralCore = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee10() {
          var utxos;
          return runtime_1.wrap(function _callee10$(_context10) {
            while (1) {
              switch (_context10.prev = _context10.next) {
                case 0:
                  _context10.t0 = _this;
                  _context10.next = 3;
                  return _this.wallet.address();

                case 3:
                  _context10.t1 = _context10.sent;
                  _context10.next = 6;
                  return _context10.t0.utxosAt.call(_context10.t0, _context10.t1);

                case 6:
                  utxos = _context10.sent;
                  return _context10.abrupt("return", utxos.filter(function (utxo) {
                    return Object.keys(utxo.assets).length === 1 && utxo.assets.lovelace >= 5000000n;
                  }).map(function (utxo) {
                    return utxoToCore(utxo);
                  }));

                case 8:
                case "end":
                  return _context10.stop();
              }
            }
          }, _callee10);
        }));

        function getCollateralCore() {
          return _getCollateralCore.apply(this, arguments);
        }

        return getCollateralCore;
      }(),
      getUtxos: function () {
        var _getUtxos = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee11() {
          return runtime_1.wrap(function _callee11$(_context11) {
            while (1) {
              switch (_context11.prev = _context11.next) {
                case 0:
                  _context11.t0 = _this;
                  _context11.next = 3;
                  return _this.wallet.address();

                case 3:
                  _context11.t1 = _context11.sent;
                  _context11.next = 6;
                  return _context11.t0.utxosAt.call(_context11.t0, _context11.t1);

                case 6:
                  return _context11.abrupt("return", _context11.sent);

                case 7:
                case "end":
                  return _context11.stop();
              }
            }
          }, _callee11);
        }));

        function getUtxos() {
          return _getUtxos.apply(this, arguments);
        }

        return getUtxos;
      }(),
      getUtxosCore: function () {
        var _getUtxosCore = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee12() {
          var utxos, coreUtxos;
          return runtime_1.wrap(function _callee12$(_context12) {
            while (1) {
              switch (_context12.prev = _context12.next) {
                case 0:
                  _context12.t0 = _this;
                  _context12.next = 3;
                  return _this.wallet.address();

                case 3:
                  _context12.t1 = _context12.sent;
                  _context12.next = 6;
                  return _context12.t0.utxosAt.call(_context12.t0, _context12.t1);

                case 6:
                  utxos = _context12.sent;
                  coreUtxos = C.TransactionUnspentOutputs["new"]();
                  utxos.forEach(function (utxo) {
                    coreUtxos.add(utxoToCore(utxo));
                  });
                  return _context12.abrupt("return", coreUtxos);

                case 10:
                case "end":
                  return _context12.stop();
              }
            }
          }, _callee12);
        }));

        function getUtxosCore() {
          return _getUtxosCore.apply(this, arguments);
        }

        return getUtxosCore;
      }(),
      signTx: function () {
        var _signTx = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee13(tx) {
          var witness, txWitnessSetBuilder;
          return runtime_1.wrap(function _callee13$(_context13) {
            while (1) {
              switch (_context13.prev = _context13.next) {
                case 0:
                  witness = C.make_vkey_witness(C.hash_transaction(tx.body()), priv);
                  txWitnessSetBuilder = C.TransactionWitnessSetBuilder["new"]();
                  txWitnessSetBuilder.add_vkey(witness);
                  return _context13.abrupt("return", txWitnessSetBuilder.build());

                case 4:
                case "end":
                  return _context13.stop();
              }
            }
          }, _callee13);
        }));

        function signTx(_x8) {
          return _signTx.apply(this, arguments);
        }

        return signTx;
      }(),
      submitTx: function () {
        var _submitTx = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee14(tx) {
          return runtime_1.wrap(function _callee14$(_context14) {
            while (1) {
              switch (_context14.prev = _context14.next) {
                case 0:
                  _context14.next = 2;
                  return _this.provider.submitTx(tx);

                case 2:
                  return _context14.abrupt("return", _context14.sent);

                case 3:
                case "end":
                  return _context14.stop();
              }
            }
          }, _callee14);
        }));

        function submitTx(_x9) {
          return _submitTx.apply(this, arguments);
        }

        return submitTx;
      }()
    };
    return this;
  };

  _proto.selectWallet = function selectWallet(api) {
    this.wallet = {
      address: function () {
        var _address2 = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee15() {
          return runtime_1.wrap(function _callee15$(_context15) {
            while (1) {
              switch (_context15.prev = _context15.next) {
                case 0:
                  _context15.t0 = C.Address;
                  _context15.t1 = fromHex;
                  _context15.next = 4;
                  return api.getUsedAddresses();

                case 4:
                  _context15.t2 = _context15.sent[0];
                  _context15.t3 = (0, _context15.t1)(_context15.t2);
                  return _context15.abrupt("return", _context15.t0.from_bytes.call(_context15.t0, _context15.t3).to_bech32());

                case 7:
                case "end":
                  return _context15.stop();
              }
            }
          }, _callee15);
        }));

        function address() {
          return _address2.apply(this, arguments);
        }

        return address;
      }(),
      rewardAddress: function () {
        var _rewardAddress2 = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee16() {
          var _yield$api$getRewardA, rewardAddressHex, rewardAddress;

          return runtime_1.wrap(function _callee16$(_context16) {
            while (1) {
              switch (_context16.prev = _context16.next) {
                case 0:
                  _context16.next = 2;
                  return api.getRewardAddresses();

                case 2:
                  _yield$api$getRewardA = _context16.sent;
                  rewardAddressHex = _yield$api$getRewardA[0];
                  rewardAddress = rewardAddressHex != null ? rewardAddressHex : C.RewardAddress.from_address(C.Address.from_bytes(fromHex(rewardAddressHex))).to_address().to_bech32();
                  return _context16.abrupt("return", rewardAddress);

                case 6:
                case "end":
                  return _context16.stop();
              }
            }
          }, _callee16);
        }));

        function rewardAddress() {
          return _rewardAddress2.apply(this, arguments);
        }

        return rewardAddress;
      }(),
      getCollateral: function () {
        var _getCollateral2 = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee17() {
          var utxos;
          return runtime_1.wrap(function _callee17$(_context17) {
            while (1) {
              switch (_context17.prev = _context17.next) {
                case 0:
                  _context17.next = 2;
                  return api.experimental.getCollateral();

                case 2:
                  utxos = _context17.sent.map(function (utxo) {
                    var parsedUtxo = C.TransactionUnspentOutput.from_bytes(fromHex(utxo));
                    return coreToUtxo(parsedUtxo);
                  });
                  return _context17.abrupt("return", utxos);

                case 4:
                case "end":
                  return _context17.stop();
              }
            }
          }, _callee17);
        }));

        function getCollateral() {
          return _getCollateral2.apply(this, arguments);
        }

        return getCollateral;
      }(),
      getCollateralCore: function () {
        var _getCollateralCore2 = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee18() {
          var utxos;
          return runtime_1.wrap(function _callee18$(_context18) {
            while (1) {
              switch (_context18.prev = _context18.next) {
                case 0:
                  _context18.next = 2;
                  return api.experimental.getCollateral();

                case 2:
                  utxos = _context18.sent.map(function (utxo) {
                    return C.TransactionUnspentOutput.from_bytes(fromHex(utxo));
                  });
                  return _context18.abrupt("return", utxos);

                case 4:
                case "end":
                  return _context18.stop();
              }
            }
          }, _callee18);
        }));

        function getCollateralCore() {
          return _getCollateralCore2.apply(this, arguments);
        }

        return getCollateralCore;
      }(),
      getUtxos: function () {
        var _getUtxos2 = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee19() {
          var utxos;
          return runtime_1.wrap(function _callee19$(_context19) {
            while (1) {
              switch (_context19.prev = _context19.next) {
                case 0:
                  _context19.next = 2;
                  return api.getUtxos();

                case 2:
                  _context19.t0 = _context19.sent;

                  if (_context19.t0) {
                    _context19.next = 5;
                    break;
                  }

                  _context19.t0 = [];

                case 5:
                  utxos = _context19.t0.map(function (utxo) {
                    var parsedUtxo = C.TransactionUnspentOutput.from_bytes(fromHex(utxo));
                    return coreToUtxo(parsedUtxo);
                  });
                  return _context19.abrupt("return", utxos);

                case 7:
                case "end":
                  return _context19.stop();
              }
            }
          }, _callee19);
        }));

        function getUtxos() {
          return _getUtxos2.apply(this, arguments);
        }

        return getUtxos;
      }(),
      getUtxosCore: function () {
        var _getUtxosCore2 = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee20() {
          var utxos;
          return runtime_1.wrap(function _callee20$(_context20) {
            while (1) {
              switch (_context20.prev = _context20.next) {
                case 0:
                  utxos = C.TransactionUnspentOutputs["new"]();
                  _context20.next = 3;
                  return api.getUtxos();

                case 3:
                  _context20.t0 = _context20.sent;

                  if (_context20.t0) {
                    _context20.next = 6;
                    break;
                  }

                  _context20.t0 = [];

                case 6:
                  _context20.t0.forEach(function (utxo) {
                    utxos.add(C.TransactionUnspentOutput.from_bytes(fromHex(utxo)));
                  });

                  return _context20.abrupt("return", utxos);

                case 8:
                case "end":
                  return _context20.stop();
              }
            }
          }, _callee20);
        }));

        function getUtxosCore() {
          return _getUtxosCore2.apply(this, arguments);
        }

        return getUtxosCore;
      }(),
      signTx: function () {
        var _signTx2 = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee21(tx) {
          var witnessSet;
          return runtime_1.wrap(function _callee21$(_context21) {
            while (1) {
              switch (_context21.prev = _context21.next) {
                case 0:
                  _context21.next = 2;
                  return api.signTx(toHex(tx.to_bytes()), true);

                case 2:
                  witnessSet = _context21.sent;
                  return _context21.abrupt("return", C.TransactionWitnessSet.from_bytes(fromHex(witnessSet)));

                case 4:
                case "end":
                  return _context21.stop();
              }
            }
          }, _callee21);
        }));

        function signTx(_x10) {
          return _signTx2.apply(this, arguments);
        }

        return signTx;
      }(),
      submitTx: function () {
        var _submitTx2 = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee22(tx) {
          var txHash;
          return runtime_1.wrap(function _callee22$(_context22) {
            while (1) {
              switch (_context22.prev = _context22.next) {
                case 0:
                  _context22.next = 2;
                  return api.submitTx(toHex(tx.to_bytes()));

                case 2:
                  txHash = _context22.sent;
                  return _context22.abrupt("return", txHash);

                case 4:
                case "end":
                  return _context22.stop();
              }
            }
          }, _callee22);
        }));

        function submitTx(_x11) {
          return _submitTx2.apply(this, arguments);
        }

        return submitTx;
      }()
    };
    return this;
  }
  /**
   * Emulates a CIP30 wallet by constructing it
   * with the UTxOs, collateral and addresses.
   *
   * If utxos are not set, utxos are fetched from the provided address
   */
  ;

  _proto.selectWalletFrom = function selectWalletFrom(_ref) {
    var _this2 = this;

    var _address3 = _ref.address,
        utxos = _ref.utxos,
        collateral = _ref.collateral,
        _rewardAddress3 = _ref.rewardAddress;
    var addressDetails = this.utils.getAddressDetails(_address3);
    this.wallet = {
      address: function () {
        var _address4 = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee23() {
          return runtime_1.wrap(function _callee23$(_context23) {
            while (1) {
              switch (_context23.prev = _context23.next) {
                case 0:
                  return _context23.abrupt("return", _address3);

                case 1:
                case "end":
                  return _context23.stop();
              }
            }
          }, _callee23);
        }));

        function address() {
          return _address4.apply(this, arguments);
        }

        return address;
      }(),
      rewardAddress: function () {
        var _rewardAddress4 = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee24() {
          var rewardAddr;
          return runtime_1.wrap(function _callee24$(_context24) {
            while (1) {
              switch (_context24.prev = _context24.next) {
                case 0:
                  rewardAddr = !_rewardAddress3 && addressDetails.stakeCredential ? function () {
                    if (addressDetails.stakeCredential.type === 'Key') {
                      return C.RewardAddress["new"](_this2.network === 'Mainnet' ? 1 : 0, C.StakeCredential.from_keyhash(C.Ed25519KeyHash.from_hex(addressDetails.stakeCredential.hash))).to_address().to_bech32();
                    }

                    return C.RewardAddress["new"](_this2.network === 'Mainnet' ? 1 : 0, C.StakeCredential.from_scripthash(C.ScriptHash.from_hex(addressDetails.stakeCredential.hash))).to_address().to_bech32();
                  }() : _rewardAddress3;
                  return _context24.abrupt("return", rewardAddr);

                case 2:
                case "end":
                  return _context24.stop();
              }
            }
          }, _callee24);
        }));

        function rewardAddress() {
          return _rewardAddress4.apply(this, arguments);
        }

        return rewardAddress;
      }(),
      getCollateral: function () {
        var _getCollateral3 = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee25() {
          return runtime_1.wrap(function _callee25$(_context25) {
            while (1) {
              switch (_context25.prev = _context25.next) {
                case 0:
                  return _context25.abrupt("return", collateral ? collateral : []);

                case 1:
                case "end":
                  return _context25.stop();
              }
            }
          }, _callee25);
        }));

        function getCollateral() {
          return _getCollateral3.apply(this, arguments);
        }

        return getCollateral;
      }(),
      getCollateralCore: function () {
        var _getCollateralCore3 = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee26() {
          return runtime_1.wrap(function _callee26$(_context26) {
            while (1) {
              switch (_context26.prev = _context26.next) {
                case 0:
                  return _context26.abrupt("return", collateral ? collateral.map(function (utxo) {
                    return utxoToCore(utxo);
                  }) : []);

                case 1:
                case "end":
                  return _context26.stop();
              }
            }
          }, _callee26);
        }));

        function getCollateralCore() {
          return _getCollateralCore3.apply(this, arguments);
        }

        return getCollateralCore;
      }(),
      getUtxos: function () {
        var _getUtxos3 = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee27() {
          return runtime_1.wrap(function _callee27$(_context27) {
            while (1) {
              switch (_context27.prev = _context27.next) {
                case 0:
                  if (!utxos) {
                    _context27.next = 4;
                    break;
                  }

                  _context27.t0 = utxos;
                  _context27.next = 7;
                  break;

                case 4:
                  _context27.next = 6;
                  return _this2.utxosAt(_address3);

                case 6:
                  _context27.t0 = _context27.sent;

                case 7:
                  return _context27.abrupt("return", _context27.t0);

                case 8:
                case "end":
                  return _context27.stop();
              }
            }
          }, _callee27);
        }));

        function getUtxos() {
          return _getUtxos3.apply(this, arguments);
        }

        return getUtxos;
      }(),
      getUtxosCore: function () {
        var _getUtxosCore3 = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee28() {
          var coreUtxos;
          return runtime_1.wrap(function _callee28$(_context28) {
            while (1) {
              switch (_context28.prev = _context28.next) {
                case 0:
                  coreUtxos = C.TransactionUnspentOutputs["new"]();

                  if (!utxos) {
                    _context28.next = 5;
                    break;
                  }

                  _context28.t0 = utxos;
                  _context28.next = 8;
                  break;

                case 5:
                  _context28.next = 7;
                  return _this2.utxosAt(_address3);

                case 7:
                  _context28.t0 = _context28.sent;

                case 8:
                  _context28.t0.forEach(function (utxo) {
                    return coreUtxos.add(utxoToCore(utxo));
                  });

                  return _context28.abrupt("return", coreUtxos);

                case 10:
                case "end":
                  return _context28.stop();
              }
            }
          }, _callee28);
        }));

        function getUtxosCore() {
          return _getUtxosCore3.apply(this, arguments);
        }

        return getUtxosCore;
      }(),
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      signTx: function () {
        var _signTx3 = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee29(_) {
          return runtime_1.wrap(function _callee29$(_context29) {
            while (1) {
              switch (_context29.prev = _context29.next) {
                case 0:
                  throw new Error('Not implemented');

                case 1:
                case "end":
                  return _context29.stop();
              }
            }
          }, _callee29);
        }));

        function signTx(_x12) {
          return _signTx3.apply(this, arguments);
        }

        return signTx;
      }(),
      submitTx: function () {
        var _submitTx3 = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee30(tx) {
          return runtime_1.wrap(function _callee30$(_context30) {
            while (1) {
              switch (_context30.prev = _context30.next) {
                case 0:
                  _context30.next = 2;
                  return _this2.provider.submitTx(tx);

                case 2:
                  return _context30.abrupt("return", _context30.sent);

                case 3:
                case "end":
                  return _context30.stop();
              }
            }
          }, _callee30);
        }));

        function submitTx(_x13) {
          return _submitTx3.apply(this, arguments);
        }

        return submitTx;
      }()
    };
    return this;
  };

  return Lucid;
}();

if (typeof window === 'undefined') {
  var fetch$1 = await /*#__PURE__*/import(
  /* webpackIgnore: true */
  'node-fetch'); // @ts-ignore

  global.fetch = fetch$1["default"]; // @ts-ignore

  global.Headers = fetch$1.Headers; // @ts-ignore

  global.Request = fetch$1.Request; // @ts-ignore

  global.Response = fetch$1.Response;
}

var Blockfrost = /*#__PURE__*/function () {
  function Blockfrost(url, projectId) {
    this.url = url;
    this.projectId = projectId;
  }

  var _proto = Blockfrost.prototype;

  _proto.getProtocolParameters = /*#__PURE__*/function () {
    var _getProtocolParameters = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee() {
      var result;
      return runtime_1.wrap(function _callee$(_context) {
        while (1) {
          switch (_context.prev = _context.next) {
            case 0:
              _context.next = 2;
              return fetch(this.url + "/epochs/latest/parameters", {
                headers: {
                  project_id: this.projectId
                }
              }).then(function (res) {
                return res.json();
              });

            case 2:
              result = _context.sent;
              return _context.abrupt("return", {
                minFeeA: parseInt(result.min_fee_a),
                minFeeB: parseInt(result.min_fee_b),
                maxTxSize: parseInt(result.max_tx_size),
                maxValSize: parseInt(result.max_val_size),
                keyDeposit: BigInt(result.key_deposit),
                poolDeposit: BigInt(result.pool_deposit),
                priceMem: parseFloat(result.price_mem),
                priceStep: parseFloat(result.price_step),
                coinsPerUtxoByte: BigInt(result.coins_per_utxo_size),
                collateralPercentage: parseInt(result.collateral_percent),
                maxCollateralInputs: parseInt(result.max_collateral_inputs)
              });

            case 4:
            case "end":
              return _context.stop();
          }
        }
      }, _callee, this);
    }));

    function getProtocolParameters() {
      return _getProtocolParameters.apply(this, arguments);
    }

    return getProtocolParameters;
  }();

  _proto.getCurrentSlot = /*#__PURE__*/function () {
    var _getCurrentSlot = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee2() {
      return runtime_1.wrap(function _callee2$(_context2) {
        while (1) {
          switch (_context2.prev = _context2.next) {
            case 0:
              _context2.next = 2;
              return fetch(this.url + "/blocks/latest", {
                headers: {
                  project_id: this.projectId
                }
              }).then(function (res) {
                return res.json();
              }).then(function (res) {
                return parseInt(res.slot);
              });

            case 2:
              return _context2.abrupt("return", _context2.sent);

            case 3:
            case "end":
              return _context2.stop();
          }
        }
      }, _callee2, this);
    }));

    function getCurrentSlot() {
      return _getCurrentSlot.apply(this, arguments);
    }

    return getCurrentSlot;
  }();

  _proto.getUtxos = /*#__PURE__*/function () {
    var _getUtxos = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee5(address) {
      var _this = this;

      var result, page, pageResult;
      return runtime_1.wrap(function _callee5$(_context5) {
        while (1) {
          switch (_context5.prev = _context5.next) {
            case 0:
              result = [];
              page = 1;
              /*eslint no-constant-condition: ["error", { "checkLoops": false }]*/

            case 2:

              _context5.next = 5;
              return fetch(this.url + "/addresses/" + address + "/utxos?page=" + page, {
                headers: {
                  project_id: this.projectId
                }
              }).then(function (res) {
                return res.json();
              });

            case 5:
              pageResult = _context5.sent;

              if (!pageResult.error) {
                _context5.next = 16;
                break;
              }

              if (!(result.status_code === 400)) {
                _context5.next = 11;
                break;
              }

              return _context5.abrupt("return", []);

            case 11:
              if (!(result.status_code === 500)) {
                _context5.next = 15;
                break;
              }

              return _context5.abrupt("return", []);

            case 15:
              pageResult = [];

            case 16:
              result = result.concat(pageResult);

              if (!(pageResult.length <= 0)) {
                _context5.next = 19;
                break;
              }

              return _context5.abrupt("break", 22);

            case 19:
              page++;
              _context5.next = 2;
              break;

            case 22:
              return _context5.abrupt("return", Promise.all(result.map( /*#__PURE__*/function () {
                var _ref = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee4(r) {
                  return runtime_1.wrap(function _callee4$(_context4) {
                    while (1) {
                      switch (_context4.prev = _context4.next) {
                        case 0:
                          _context4.t0 = r.tx_hash;
                          _context4.t1 = r.output_index;

                          _context4.t2 = function () {
                            var a = {};
                            r.amount.forEach(function (am) {
                              a[am.unit] = BigInt(am.quantity);
                            });
                            return a;
                          }();

                          _context4.t3 = address;
                          _context4.t4 = !r.inline_datum ? r.data_hash : null;
                          _context4.t5 = r.inline_datum;
                          _context4.t6 = r.reference_script_hash;

                          if (!_context4.t6) {
                            _context4.next = 11;
                            break;
                          }

                          _context4.next = 10;
                          return _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee3() {
                            var _yield$fetch$then, type, _yield$fetch$then2, cbor, script, scriptRef;

                            return runtime_1.wrap(function _callee3$(_context3) {
                              while (1) {
                                switch (_context3.prev = _context3.next) {
                                  case 0:
                                    _context3.next = 2;
                                    return fetch(_this.url + "/scripts/" + r.reference_script_hash, {
                                      headers: {
                                        project_id: _this.projectId
                                      }
                                    }).then(function (res) {
                                      return res.json();
                                    });

                                  case 2:
                                    _yield$fetch$then = _context3.sent;
                                    type = _yield$fetch$then.type;

                                    if (!(type === 'Native')) {
                                      _context3.next = 6;
                                      break;
                                    }

                                    throw new Error('Native script ref not implemented!');

                                  case 6:
                                    _context3.next = 8;
                                    return fetch(_this.url + "/scripts/" + r.reference_script_hash + "/cbor", {
                                      headers: {
                                        project_id: _this.projectId
                                      }
                                    }).then(function (res) {
                                      return res.json();
                                    });

                                  case 8:
                                    _yield$fetch$then2 = _context3.sent;
                                    cbor = _yield$fetch$then2.cbor;
                                    script = C.PlutusScript.from_bytes(fromHex(cbor));
                                    scriptRef = C.ScriptRef["new"](type === 'PlutusV1' ? C.Script.new_plutus_v1(script) : C.Script.new_plutus_v2(script));
                                    return _context3.abrupt("return", toHex(scriptRef.to_bytes()));

                                  case 13:
                                  case "end":
                                    return _context3.stop();
                                }
                              }
                            }, _callee3);
                          }))();

                        case 10:
                          _context4.t6 = _context4.sent;

                        case 11:
                          _context4.t7 = _context4.t6;
                          return _context4.abrupt("return", {
                            txHash: _context4.t0,
                            outputIndex: _context4.t1,
                            assets: _context4.t2,
                            address: _context4.t3,
                            datumHash: _context4.t4,
                            datum: _context4.t5,
                            scriptRef: _context4.t7
                          });

                        case 13:
                        case "end":
                          return _context4.stop();
                      }
                    }
                  }, _callee4);
                }));

                return function (_x2) {
                  return _ref.apply(this, arguments);
                };
              }())));

            case 23:
            case "end":
              return _context5.stop();
          }
        }
      }, _callee5, this);
    }));

    function getUtxos(_x) {
      return _getUtxos.apply(this, arguments);
    }

    return getUtxos;
  }();

  _proto.getUtxosWithUnit = /*#__PURE__*/function () {
    var _getUtxosWithUnit = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee8(address, unit) {
      var _this2 = this;

      var result, page, pageResult;
      return runtime_1.wrap(function _callee8$(_context8) {
        while (1) {
          switch (_context8.prev = _context8.next) {
            case 0:
              result = [];
              page = 1;

            case 2:

              _context8.next = 5;
              return fetch(this.url + "/addresses/" + address + "/utxos/" + unit + "?page=" + page, {
                headers: {
                  project_id: this.projectId
                }
              }).then(function (res) {
                return res.json();
              });

            case 5:
              pageResult = _context8.sent;

              if (!pageResult.error) {
                _context8.next = 16;
                break;
              }

              if (!(result.status_code === 400)) {
                _context8.next = 11;
                break;
              }

              return _context8.abrupt("return", []);

            case 11:
              if (!(result.status_code === 500)) {
                _context8.next = 15;
                break;
              }

              return _context8.abrupt("return", []);

            case 15:
              pageResult = [];

            case 16:
              result = result.concat(pageResult);

              if (!(pageResult.length <= 0)) {
                _context8.next = 19;
                break;
              }

              return _context8.abrupt("break", 22);

            case 19:
              page++;
              _context8.next = 2;
              break;

            case 22:
              return _context8.abrupt("return", Promise.all(result.map( /*#__PURE__*/function () {
                var _ref3 = _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee7(r) {
                  return runtime_1.wrap(function _callee7$(_context7) {
                    while (1) {
                      switch (_context7.prev = _context7.next) {
                        case 0:
                          _context7.t0 = r.tx_hash;
                          _context7.t1 = r.output_index;

                          _context7.t2 = function () {
                            var a = {};
                            r.amount.forEach(function (am) {
                              a[am.unit] = BigInt(am.quantity);
                            });
                            return a;
                          }();

                          _context7.t3 = address;
                          _context7.t4 = !r.inline_datum ? r.data_hash : null;
                          _context7.t5 = r.inline_datum;
                          _context7.t6 = r.reference_script_hash;

                          if (!_context7.t6) {
                            _context7.next = 11;
                            break;
                          }

                          _context7.next = 10;
                          return _asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee6() {
                            var _yield$fetch$then3, type, _yield$fetch$then4, cbor, script, scriptRef;

                            return runtime_1.wrap(function _callee6$(_context6) {
                              while (1) {
                                switch (_context6.prev = _context6.next) {
                                  case 0:
                                    _context6.next = 2;
                                    return fetch(_this2.url + "/scripts/" + r.reference_script_hash, {
                                      headers: {
                                        project_id: _this2.projectId
                                      }
                                    }).then(function (res) {
                                      return res.json();
                                    });

                                  case 2:
                                    _yield$fetch$then3 = _context6.sent;
                                    type = _yield$fetch$then3.type;

                                    if (!(type === 'Native')) {
                                      _context6.next = 6;
                                      break;
                                    }

                                    throw new Error('Native script ref not implemented!');

                                  case 6:
                                    _context6.next = 8;
                                    return fetch(_this2.url + "/scripts/" + r.reference_script_hash + "/cbor", {
                                      headers: {
                                        project_id: _this2.projectId
                                      }
                                    }).then(function (res) {
                                      return res.json();
                                    });

                                  case 8:
                                    _yield$fetch$then4 = _context6.sent;
                                    cbor = _yield$fetch$then4.cbor;
                                    script = C.PlutusScript.from_bytes(fromHex(cbor));
                                    scriptRef = C.ScriptRef["new"](type === 'PlutusV1' ? C.Script.new_plutus_v1(script) : C.Script.new_plutus_v2(script));
                                    return _context6.abrupt("return", toHex(scriptRef.to_bytes()));

                                  case 13:
                                  case "end":
                                    return _context6.stop();
                                }
                              }
                            }, _callee6);
                          }))();

                        case 10:
                          _context7.t6 = _context7.sent;

                        case 11:
                          _context7.t7 = _context7.t6;
                          return _context7.abrupt("return", {
                            txHash: _context7.t0,
                            outputIndex: _context7.t1,
                            assets: _context7.t2,
                            address: _context7.t3,
                            datumHash: _context7.t4,
                            datum: _context7.t5,
                            scriptRef: _context7.t7
                          });

                        case 13:
                        case "end":
                          return _context7.stop();
                      }
                    }
                  }, _callee7);
                }));

                return function (_x5) {
                  return _ref3.apply(this, arguments);
                };
              }())));

            case 23:
            case "end":
              return _context8.stop();
          }
        }
      }, _callee8, this);
    }));

    function getUtxosWithUnit(_x3, _x4) {
      return _getUtxosWithUnit.apply(this, arguments);
    }

    return getUtxosWithUnit;
  }();

  _proto.getDatum = /*#__PURE__*/function () {
    var _getDatum = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee9(datumHash) {
      var datum;
      return runtime_1.wrap(function _callee9$(_context9) {
        while (1) {
          switch (_context9.prev = _context9.next) {
            case 0:
              _context9.next = 2;
              return fetch(this.url + "/scripts/datum/" + datumHash, {
                headers: {
                  project_id: this.projectId
                }
              }).then(function (res) {
                return res.json();
              }).then(function (res) {
                return res.json_value;
              });

            case 2:
              datum = _context9.sent;

              if (!(!datum || datum.error)) {
                _context9.next = 5;
                break;
              }

              throw new Error("No datum found for datum hash: " + datumHash);

            case 5:
              return _context9.abrupt("return", datumJsonToCbor(datum));

            case 6:
            case "end":
              return _context9.stop();
          }
        }
      }, _callee9, this);
    }));

    function getDatum(_x6) {
      return _getDatum.apply(this, arguments);
    }

    return getDatum;
  }();

  _proto.awaitTx = /*#__PURE__*/function () {
    var _awaitTx = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee11(txHash) {
      var _this3 = this;

      return runtime_1.wrap(function _callee11$(_context11) {
        while (1) {
          switch (_context11.prev = _context11.next) {
            case 0:
              return _context11.abrupt("return", new Promise(function (res) {
                var confirmation = setInterval( /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee10() {
                  var isConfirmed;
                  return runtime_1.wrap(function _callee10$(_context10) {
                    while (1) {
                      switch (_context10.prev = _context10.next) {
                        case 0:
                          _context10.next = 2;
                          return fetch(_this3.url + "/txs/" + txHash, {
                            headers: {
                              project_id: _this3.projectId
                            }
                          }).then(function (res) {
                            return res.json();
                          });

                        case 2:
                          isConfirmed = _context10.sent;

                          if (!(isConfirmed && !isConfirmed.error)) {
                            _context10.next = 7;
                            break;
                          }

                          clearInterval(confirmation);
                          res(true);
                          return _context10.abrupt("return");

                        case 7:
                        case "end":
                          return _context10.stop();
                      }
                    }
                  }, _callee10);
                })), 3000);
              }));

            case 1:
            case "end":
              return _context11.stop();
          }
        }
      }, _callee11);
    }));

    function awaitTx(_x7) {
      return _awaitTx.apply(this, arguments);
    }

    return awaitTx;
  }();

  _proto.submitTx = /*#__PURE__*/function () {
    var _submitTx = /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/runtime_1.mark(function _callee12(tx) {
      var result;
      return runtime_1.wrap(function _callee12$(_context12) {
        while (1) {
          switch (_context12.prev = _context12.next) {
            case 0:
              _context12.next = 2;
              return fetch(this.url + "/tx/submit", {
                method: 'POST',
                headers: {
                  'Content-Type': 'application/cbor',
                  project_id: this.projectId
                },
                body: tx.to_bytes()
              }).then(function (res) {
                return res.json();
              });

            case 2:
              result = _context12.sent;

              if (!(!result || result.error)) {
                _context12.next = 9;
                break;
              }

              if (!((result == null ? void 0 : result.status_code) === 400)) {
                _context12.next = 8;
                break;
              }

              throw new Error(result.message);

            case 8:
              throw new Error('Could not submit transaction.');

            case 9:
              return _context12.abrupt("return", result);

            case 10:
            case "end":
              return _context12.stop();
          }
        }
      }, _callee12, this);
    }));

    function submitTx(_x8) {
      return _submitTx.apply(this, arguments);
    }

    return submitTx;
  }();

  return Blockfrost;
}();
/** This function is temporarily needed only, until Blockfrost returns the datum natively in cbor
 *
 * The conversion is ambigious, that's why it's better to get the datum directly in cbor
 */

var datumJsonToCbor = function datumJsonToCbor(json) {
  var convert = function convert(json) {
    if (!isNaN(json["int"])) {
      return C.PlutusData.new_integer(C.BigInt.from_str(json["int"].toString()));
    } else if (json.bytes || !isNaN(json.bytes)) {
      return C.PlutusData.new_bytes(fromHex(json.bytes));
    } else if (json.map) {
      var m = C.PlutusMap["new"]();
      json.map.forEach(function (_ref6) {
        var v = _ref6.v,
            k = _ref6.k;
        m.insert(convert(k), convert(v));
      });
      return C.PlutusData.new_map(m);
    } else if (json.list) {
      var l = C.PlutusList["new"]();
      json.list.forEach(function (v) {
        l.add(convert(v));
      });
      return C.PlutusData.new_list(l);
    } else if (!isNaN(json.constructor)) {
      var _l = C.PlutusList["new"]();

      json.fields.forEach(function (v) {
        _l.add(convert(v));
      });
      return C.PlutusData.new_constr_plutus_data(C.ConstrPlutusData["new"](C.BigNum.from_str(json.constructor.toString()), _l));
    }

    throw new Error('Unsupported type');
  };

  return toHex(convert(json).to_bytes());
};

export { Blockfrost, C, Construct, Data, Lucid, Tx, TxComplete, TxSigned, Utils, assetsToValue, coreToUtxo, costModels, datumJsonToCbor, fromHex, toHex, utxoToCore, valueToAssets };
