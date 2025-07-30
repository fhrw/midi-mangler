(() => {
  var __defProp = Object.defineProperty;
  var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __hasOwnProp = Object.prototype.hasOwnProperty;
  var __esm = (fn, res) => function __init() {
    return fn && (res = (0, fn[__getOwnPropNames(fn)[0]])(fn = 0)), res;
  };
  var __export = (target6, all3) => {
    for (var name15 in all3)
      __defProp(target6, name15, { get: all3[name15], enumerable: true });
  };
  var __copyProps = (to, from3, except2, desc) => {
    if (from3 && typeof from3 === "object" || typeof from3 === "function") {
      for (let key of __getOwnPropNames(from3))
        if (!__hasOwnProp.call(to, key) && key !== except2)
          __defProp(to, key, { get: () => from3[key], enumerable: !(desc = __getOwnPropDesc(from3, key)) || desc.enumerable });
    }
    return to;
  };
  var __toCommonJS = (mod2) => __copyProps(__defProp({}, "__esModule", { value: true }), mod2);

  // output/Main/foreign.js
  var readFileFromFilePickEvent;
  var init_foreign = __esm({
    "output/Main/foreign.js"() {
      readFileFromFilePickEvent = function(args) {
        return function(onError, onSuccess) {
          const fileInput = args.event.target;
          if (!fileInput.files || fileInput.files.length === 0) {
            alert("No file selected");
            return onSuccess(args.nothing);
          }
          const selectedFile = fileInput.files[0];
          const fileReader = new FileReader();
          fileReader.onload = function(e) {
            const fileContents = e.target.result;
            let ui8 = new Uint8Array(fileContents);
            onSuccess(args.just([...ui8]));
          };
          fileReader.readAsArrayBuffer(selectedFile);
          return function(cancelError, onCancelerError, onCancelerSuccess) {
            onCancelerSuccess();
          };
        };
      };
    }
  });

  // output/Control.Apply/foreign.js
  var arrayApply;
  var init_foreign2 = __esm({
    "output/Control.Apply/foreign.js"() {
      arrayApply = function(fs) {
        return function(xs) {
          var l = fs.length;
          var k = xs.length;
          var result = new Array(l * k);
          var n = 0;
          for (var i2 = 0; i2 < l; i2++) {
            var f = fs[i2];
            for (var j = 0; j < k; j++) {
              result[n++] = f(xs[j]);
            }
          }
          return result;
        };
      };
    }
  });

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn;
  var init_Control = __esm({
    "output/Control.Semigroupoid/index.js"() {
      semigroupoidFn = {
        compose: function(f) {
          return function(g) {
            return function(x) {
              return f(g(x));
            };
          };
        }
      };
    }
  });

  // output/Control.Category/index.js
  var identity, categoryFn;
  var init_Control2 = __esm({
    "output/Control.Category/index.js"() {
      init_Control();
      init_Control();
      identity = function(dict) {
        return dict.identity;
      };
      categoryFn = {
        identity: function(x) {
          return x;
        },
        Semigroupoid0: function() {
          return semigroupoidFn;
        }
      };
    }
  });

  // output/Data.Boolean/index.js
  var otherwise;
  var init_Data = __esm({
    "output/Data.Boolean/index.js"() {
      otherwise = true;
    }
  });

  // output/Data.Function/index.js
  var flip, $$const;
  var init_Data2 = __esm({
    "output/Data.Function/index.js"() {
      init_Control2();
      init_Data();
      init_Control2();
      flip = function(f) {
        return function(b2) {
          return function(a2) {
            return f(a2)(b2);
          };
        };
      };
      $$const = function(a2) {
        return function(v) {
          return a2;
        };
      };
    }
  });

  // output/Data.Functor/foreign.js
  var arrayMap;
  var init_foreign3 = __esm({
    "output/Data.Functor/foreign.js"() {
      arrayMap = function(f) {
        return function(arr) {
          var l = arr.length;
          var result = new Array(l);
          for (var i2 = 0; i2 < l; i2++) {
            result[i2] = f(arr[i2]);
          }
          return result;
        };
      };
    }
  });

  // output/Data.Unit/foreign.js
  var unit;
  var init_foreign4 = __esm({
    "output/Data.Unit/foreign.js"() {
      unit = void 0;
    }
  });

  // output/Data.Unit/index.js
  var init_Data3 = __esm({
    "output/Data.Unit/index.js"() {
      init_foreign4();
      init_foreign4();
    }
  });

  // output/Type.Proxy/index.js
  var $$Proxy;
  var init_Type = __esm({
    "output/Type.Proxy/index.js"() {
      $$Proxy = /* @__PURE__ */ function() {
        function $$Proxy2() {
        }
        ;
        $$Proxy2.value = new $$Proxy2();
        return $$Proxy2;
      }();
    }
  });

  // output/Data.Functor/index.js
  var map, mapFlipped, $$void, voidLeft, functorArray;
  var init_Data4 = __esm({
    "output/Data.Functor/index.js"() {
      init_foreign3();
      init_Control();
      init_Data2();
      init_Data3();
      init_Type();
      map = function(dict) {
        return dict.map;
      };
      mapFlipped = function(dictFunctor) {
        var map110 = map(dictFunctor);
        return function(fa) {
          return function(f) {
            return map110(f)(fa);
          };
        };
      };
      $$void = function(dictFunctor) {
        return map(dictFunctor)($$const(unit));
      };
      voidLeft = function(dictFunctor) {
        var map110 = map(dictFunctor);
        return function(f) {
          return function(x) {
            return map110($$const(x))(f);
          };
        };
      };
      functorArray = {
        map: arrayMap
      };
    }
  });

  // output/Control.Apply/index.js
  var identity2, applyArray, apply, applySecond;
  var init_Control3 = __esm({
    "output/Control.Apply/index.js"() {
      init_foreign2();
      init_Control2();
      init_Data2();
      init_Data4();
      init_Type();
      init_Data4();
      identity2 = /* @__PURE__ */ identity(categoryFn);
      applyArray = {
        apply: arrayApply,
        Functor0: function() {
          return functorArray;
        }
      };
      apply = function(dict) {
        return dict.apply;
      };
      applySecond = function(dictApply) {
        var apply1 = apply(dictApply);
        var map20 = map(dictApply.Functor0());
        return function(a2) {
          return function(b2) {
            return apply1(map20($$const(identity2))(a2))(b2);
          };
        };
      };
    }
  });

  // output/Control.Applicative/index.js
  var pure, unless, when, liftA1;
  var init_Control4 = __esm({
    "output/Control.Applicative/index.js"() {
      init_Control3();
      init_Data4();
      init_Data3();
      init_Type();
      init_Control3();
      init_Data4();
      pure = function(dict) {
        return dict.pure;
      };
      unless = function(dictApplicative) {
        var pure13 = pure(dictApplicative);
        return function(v) {
          return function(v1) {
            if (!v) {
              return v1;
            }
            ;
            if (v) {
              return pure13(unit);
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
          };
        };
      };
      when = function(dictApplicative) {
        var pure13 = pure(dictApplicative);
        return function(v) {
          return function(v1) {
            if (v) {
              return v1;
            }
            ;
            if (!v) {
              return pure13(unit);
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
          };
        };
      };
      liftA1 = function(dictApplicative) {
        var apply2 = apply(dictApplicative.Apply0());
        var pure13 = pure(dictApplicative);
        return function(f) {
          return function(a2) {
            return apply2(pure13(f))(a2);
          };
        };
      };
    }
  });

  // output/Control.Bind/foreign.js
  var arrayBind;
  var init_foreign5 = __esm({
    "output/Control.Bind/foreign.js"() {
      arrayBind = function(arr) {
        return function(f) {
          var result = [];
          for (var i2 = 0, l = arr.length; i2 < l; i2++) {
            Array.prototype.push.apply(result, f(arr[i2]));
          }
          return result;
        };
      };
    }
  });

  // output/Control.Bind/index.js
  var discard, bindArray, bind, bindFlipped, composeKleisliFlipped, discardUnit;
  var init_Control5 = __esm({
    "output/Control.Bind/index.js"() {
      init_foreign5();
      init_Control4();
      init_Control3();
      init_Control2();
      init_Data2();
      init_Data4();
      init_Type();
      init_Control4();
      init_Control3();
      init_Data4();
      discard = function(dict) {
        return dict.discard;
      };
      bindArray = {
        bind: arrayBind,
        Apply0: function() {
          return applyArray;
        }
      };
      bind = function(dict) {
        return dict.bind;
      };
      bindFlipped = function(dictBind) {
        return flip(bind(dictBind));
      };
      composeKleisliFlipped = function(dictBind) {
        var bindFlipped12 = bindFlipped(dictBind);
        return function(f) {
          return function(g) {
            return function(a2) {
              return bindFlipped12(f)(g(a2));
            };
          };
        };
      };
      discardUnit = {
        discard: function(dictBind) {
          return bind(dictBind);
        }
      };
    }
  });

  // output/Control.Monad/index.js
  var unlessM, ap;
  var init_Control6 = __esm({
    "output/Control.Monad/index.js"() {
      init_Control4();
      init_Control3();
      init_Control5();
      init_Data4();
      init_Control4();
      init_Control3();
      init_Control5();
      init_Data4();
      unlessM = function(dictMonad) {
        var bind7 = bind(dictMonad.Bind1());
        var unless3 = unless(dictMonad.Applicative0());
        return function(mb) {
          return function(m) {
            return bind7(mb)(function(b2) {
              return unless3(b2)(m);
            });
          };
        };
      };
      ap = function(dictMonad) {
        var bind7 = bind(dictMonad.Bind1());
        var pure11 = pure(dictMonad.Applicative0());
        return function(f) {
          return function(a2) {
            return bind7(f)(function(f$prime) {
              return bind7(a2)(function(a$prime) {
                return pure11(f$prime(a$prime));
              });
            });
          };
        };
      };
    }
  });

  // output/Control.Monad.Cont.Class/index.js
  var init_Control_Monad_Cont = __esm({
    "output/Control.Monad.Cont.Class/index.js"() {
    }
  });

  // output/Data.Semigroup/foreign.js
  var concatArray;
  var init_foreign6 = __esm({
    "output/Data.Semigroup/foreign.js"() {
      concatArray = function(xs) {
        return function(ys) {
          if (xs.length === 0)
            return ys;
          if (ys.length === 0)
            return xs;
          return xs.concat(ys);
        };
      };
    }
  });

  // output/Data.Symbol/foreign.js
  var init_foreign7 = __esm({
    "output/Data.Symbol/foreign.js"() {
    }
  });

  // output/Data.Symbol/index.js
  var reflectSymbol;
  var init_Data5 = __esm({
    "output/Data.Symbol/index.js"() {
      init_foreign7();
      init_Type();
      reflectSymbol = function(dict) {
        return dict.reflectSymbol;
      };
    }
  });

  // output/Data.Void/index.js
  var init_Data6 = __esm({
    "output/Data.Void/index.js"() {
    }
  });

  // output/Record.Unsafe/foreign.js
  var unsafeGet;
  var init_foreign8 = __esm({
    "output/Record.Unsafe/foreign.js"() {
      unsafeGet = function(label5) {
        return function(rec) {
          return rec[label5];
        };
      };
    }
  });

  // output/Record.Unsafe/index.js
  var init_Record = __esm({
    "output/Record.Unsafe/index.js"() {
      init_foreign8();
      init_foreign8();
    }
  });

  // output/Data.Semigroup/index.js
  var semigroupArray, append;
  var init_Data7 = __esm({
    "output/Data.Semigroup/index.js"() {
      init_foreign6();
      init_Data5();
      init_Data3();
      init_Data6();
      init_Record();
      init_Type();
      semigroupArray = {
        append: concatArray
      };
      append = function(dict) {
        return dict.append;
      };
    }
  });

  // output/Control.Alt/index.js
  var altArray, alt;
  var init_Control7 = __esm({
    "output/Control.Alt/index.js"() {
      init_Data4();
      init_Data7();
      init_Data4();
      altArray = {
        alt: /* @__PURE__ */ append(semigroupArray),
        Functor0: function() {
          return functorArray;
        }
      };
      alt = function(dict) {
        return dict.alt;
      };
    }
  });

  // output/Data.Bounded/foreign.js
  var topChar, bottomChar, topNumber, bottomNumber;
  var init_foreign9 = __esm({
    "output/Data.Bounded/foreign.js"() {
      topChar = String.fromCharCode(65535);
      bottomChar = String.fromCharCode(0);
      topNumber = Number.POSITIVE_INFINITY;
      bottomNumber = Number.NEGATIVE_INFINITY;
    }
  });

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl, ordIntImpl, ordStringImpl, ordCharImpl;
  var init_foreign10 = __esm({
    "output/Data.Ord/foreign.js"() {
      unsafeCompareImpl = function(lt) {
        return function(eq3) {
          return function(gt) {
            return function(x) {
              return function(y) {
                return x < y ? lt : x === y ? eq3 : gt;
              };
            };
          };
        };
      };
      ordIntImpl = unsafeCompareImpl;
      ordStringImpl = unsafeCompareImpl;
      ordCharImpl = unsafeCompareImpl;
    }
  });

  // output/Data.Eq/foreign.js
  var refEq, eqIntImpl, eqCharImpl, eqStringImpl, eqArrayImpl;
  var init_foreign11 = __esm({
    "output/Data.Eq/foreign.js"() {
      refEq = function(r1) {
        return function(r2) {
          return r1 === r2;
        };
      };
      eqIntImpl = refEq;
      eqCharImpl = refEq;
      eqStringImpl = refEq;
      eqArrayImpl = function(f) {
        return function(xs) {
          return function(ys) {
            if (xs.length !== ys.length)
              return false;
            for (var i2 = 0; i2 < xs.length; i2++) {
              if (!f(xs[i2])(ys[i2]))
                return false;
            }
            return true;
          };
        };
      };
    }
  });

  // output/Data.Eq/index.js
  var eqString, eqInt, eqChar, eq, eqArray;
  var init_Data8 = __esm({
    "output/Data.Eq/index.js"() {
      init_foreign11();
      init_Data5();
      init_Record();
      init_Type();
      eqString = {
        eq: eqStringImpl
      };
      eqInt = {
        eq: eqIntImpl
      };
      eqChar = {
        eq: eqCharImpl
      };
      eq = function(dict) {
        return dict.eq;
      };
      eqArray = function(dictEq) {
        return {
          eq: eqArrayImpl(eq(dictEq))
        };
      };
    }
  });

  // output/Data.Ordering/index.js
  var LT, GT, EQ;
  var init_Data9 = __esm({
    "output/Data.Ordering/index.js"() {
      LT = /* @__PURE__ */ function() {
        function LT2() {
        }
        ;
        LT2.value = new LT2();
        return LT2;
      }();
      GT = /* @__PURE__ */ function() {
        function GT2() {
        }
        ;
        GT2.value = new GT2();
        return GT2;
      }();
      EQ = /* @__PURE__ */ function() {
        function EQ2() {
        }
        ;
        EQ2.value = new EQ2();
        return EQ2;
      }();
    }
  });

  // output/Data.Ring/foreign.js
  var intSub;
  var init_foreign12 = __esm({
    "output/Data.Ring/foreign.js"() {
      intSub = function(x) {
        return function(y) {
          return x - y | 0;
        };
      };
    }
  });

  // output/Data.Semiring/foreign.js
  var intAdd, intMul;
  var init_foreign13 = __esm({
    "output/Data.Semiring/foreign.js"() {
      intAdd = function(x) {
        return function(y) {
          return x + y | 0;
        };
      };
      intMul = function(x) {
        return function(y) {
          return x * y | 0;
        };
      };
    }
  });

  // output/Data.Semiring/index.js
  var semiringInt;
  var init_Data10 = __esm({
    "output/Data.Semiring/index.js"() {
      init_foreign13();
      init_Data5();
      init_Data3();
      init_Record();
      init_Type();
      semiringInt = {
        add: intAdd,
        zero: 0,
        mul: intMul,
        one: 1
      };
    }
  });

  // output/Data.Ring/index.js
  var ringInt;
  var init_Data11 = __esm({
    "output/Data.Ring/index.js"() {
      init_foreign12();
      init_Data10();
      init_Data5();
      init_Data3();
      init_Record();
      init_Type();
      init_Data10();
      ringInt = {
        sub: intSub,
        Semiring0: function() {
          return semiringInt;
        }
      };
    }
  });

  // output/Data.Ord/index.js
  var ordString, ordInt, ordChar, compare;
  var init_Data12 = __esm({
    "output/Data.Ord/index.js"() {
      init_foreign10();
      init_Data8();
      init_Data9();
      init_Data11();
      init_Data10();
      init_Data5();
      init_Record();
      init_Type();
      init_Data9();
      ordString = /* @__PURE__ */ function() {
        return {
          compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
          Eq0: function() {
            return eqString;
          }
        };
      }();
      ordInt = /* @__PURE__ */ function() {
        return {
          compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
          Eq0: function() {
            return eqInt;
          }
        };
      }();
      ordChar = /* @__PURE__ */ function() {
        return {
          compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
          Eq0: function() {
            return eqChar;
          }
        };
      }();
      compare = function(dict) {
        return dict.compare;
      };
    }
  });

  // output/Data.Bounded/index.js
  var top, boundedChar, bottom;
  var init_Data13 = __esm({
    "output/Data.Bounded/index.js"() {
      init_foreign9();
      init_Data12();
      init_Data9();
      init_Data5();
      init_Data3();
      init_Record();
      init_Type();
      init_Data12();
      top = function(dict) {
        return dict.top;
      };
      boundedChar = {
        top: topChar,
        bottom: bottomChar,
        Ord0: function() {
          return ordChar;
        }
      };
      bottom = function(dict) {
        return dict.bottom;
      };
    }
  });

  // output/Data.Functor.Invariant/index.js
  var init_Data_Functor = __esm({
    "output/Data.Functor.Invariant/index.js"() {
      init_Data4();
    }
  });

  // output/Data.Show/foreign.js
  var showIntImpl, showStringImpl, showArrayImpl;
  var init_foreign14 = __esm({
    "output/Data.Show/foreign.js"() {
      showIntImpl = function(n) {
        return n.toString();
      };
      showStringImpl = function(s) {
        var l = s.length;
        return '"' + s.replace(
          /[\0-\x1F\x7F"\\]/g,
          // eslint-disable-line no-control-regex
          function(c, i2) {
            switch (c) {
              case '"':
              case "\\":
                return "\\" + c;
              case "\x07":
                return "\\a";
              case "\b":
                return "\\b";
              case "\f":
                return "\\f";
              case "\n":
                return "\\n";
              case "\r":
                return "\\r";
              case "	":
                return "\\t";
              case "\v":
                return "\\v";
            }
            var k = i2 + 1;
            var empty8 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
            return "\\" + c.charCodeAt(0).toString(10) + empty8;
          }
        ) + '"';
      };
      showArrayImpl = function(f) {
        return function(xs) {
          var ss = [];
          for (var i2 = 0, l = xs.length; i2 < l; i2++) {
            ss[i2] = f(xs[i2]);
          }
          return "[" + ss.join(",") + "]";
        };
      };
    }
  });

  // output/Data.Show/index.js
  var showString, showRecordFields, showRecord, showInt, show, showArray, showRecordFieldsCons, showRecordFieldsConsNil;
  var init_Data14 = __esm({
    "output/Data.Show/index.js"() {
      init_foreign14();
      init_Data5();
      init_Data6();
      init_Record();
      init_Type();
      showString = {
        show: showStringImpl
      };
      showRecordFields = function(dict) {
        return dict.showRecordFields;
      };
      showRecord = function() {
        return function() {
          return function(dictShowRecordFields) {
            var showRecordFields1 = showRecordFields(dictShowRecordFields);
            return {
              show: function(record) {
                return "{" + (showRecordFields1($$Proxy.value)(record) + "}");
              }
            };
          };
        };
      };
      showInt = {
        show: showIntImpl
      };
      show = function(dict) {
        return dict.show;
      };
      showArray = function(dictShow) {
        return {
          show: showArrayImpl(show(dictShow))
        };
      };
      showRecordFieldsCons = function(dictIsSymbol) {
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        return function(dictShowRecordFields) {
          var showRecordFields1 = showRecordFields(dictShowRecordFields);
          return function(dictShow) {
            var show13 = show(dictShow);
            return {
              showRecordFields: function(v) {
                return function(record) {
                  var tail = showRecordFields1($$Proxy.value)(record);
                  var key = reflectSymbol2($$Proxy.value);
                  var focus3 = unsafeGet(key)(record);
                  return " " + (key + (": " + (show13(focus3) + ("," + tail))));
                };
              }
            };
          };
        };
      };
      showRecordFieldsConsNil = function(dictIsSymbol) {
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        return function(dictShow) {
          var show13 = show(dictShow);
          return {
            showRecordFields: function(v) {
              return function(record) {
                var key = reflectSymbol2($$Proxy.value);
                var focus3 = unsafeGet(key)(record);
                return " " + (key + (": " + (show13(focus3) + " ")));
              };
            }
          };
        };
      };
    }
  });

  // output/Data.Generic.Rep/index.js
  var Inl, Inr, Product, NoArguments, from;
  var init_Data_Generic = __esm({
    "output/Data.Generic.Rep/index.js"() {
      init_Data14();
      init_Data5();
      init_Type();
      Inl = /* @__PURE__ */ function() {
        function Inl2(value0) {
          this.value0 = value0;
        }
        ;
        Inl2.create = function(value0) {
          return new Inl2(value0);
        };
        return Inl2;
      }();
      Inr = /* @__PURE__ */ function() {
        function Inr2(value0) {
          this.value0 = value0;
        }
        ;
        Inr2.create = function(value0) {
          return new Inr2(value0);
        };
        return Inr2;
      }();
      Product = /* @__PURE__ */ function() {
        function Product2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        Product2.create = function(value0) {
          return function(value1) {
            return new Product2(value0, value1);
          };
        };
        return Product2;
      }();
      NoArguments = /* @__PURE__ */ function() {
        function NoArguments2() {
        }
        ;
        NoArguments2.value = new NoArguments2();
        return NoArguments2;
      }();
      from = function(dict) {
        return dict.from;
      };
    }
  });

  // output/Data.Maybe/index.js
  var identity3, Nothing, Just, maybe, isNothing, isJust, functorMaybe, map2, fromMaybe, fromJust, applyMaybe, bindMaybe;
  var init_Data15 = __esm({
    "output/Data.Maybe/index.js"() {
      init_Control7();
      init_Control4();
      init_Control3();
      init_Control2();
      init_Data13();
      init_Data8();
      init_Data2();
      init_Data4();
      init_Data_Functor();
      init_Data_Generic();
      init_Data12();
      init_Data9();
      init_Data7();
      init_Data10();
      init_Data14();
      init_Data3();
      identity3 = /* @__PURE__ */ identity(categoryFn);
      Nothing = /* @__PURE__ */ function() {
        function Nothing2() {
        }
        ;
        Nothing2.value = new Nothing2();
        return Nothing2;
      }();
      Just = /* @__PURE__ */ function() {
        function Just2(value0) {
          this.value0 = value0;
        }
        ;
        Just2.create = function(value0) {
          return new Just2(value0);
        };
        return Just2;
      }();
      maybe = function(v) {
        return function(v1) {
          return function(v2) {
            if (v2 instanceof Nothing) {
              return v;
            }
            ;
            if (v2 instanceof Just) {
              return v1(v2.value0);
            }
            ;
            throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
          };
        };
      };
      isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
      isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
      functorMaybe = {
        map: function(v) {
          return function(v1) {
            if (v1 instanceof Just) {
              return new Just(v(v1.value0));
            }
            ;
            return Nothing.value;
          };
        }
      };
      map2 = /* @__PURE__ */ map(functorMaybe);
      fromMaybe = function(a2) {
        return maybe(a2)(identity3);
      };
      fromJust = function() {
        return function(v) {
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
        };
      };
      applyMaybe = {
        apply: function(v) {
          return function(v1) {
            if (v instanceof Just) {
              return map2(v.value0)(v1);
            }
            ;
            if (v instanceof Nothing) {
              return Nothing.value;
            }
            ;
            throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
          };
        },
        Functor0: function() {
          return functorMaybe;
        }
      };
      bindMaybe = {
        bind: function(v) {
          return function(v1) {
            if (v instanceof Just) {
              return v1(v.value0);
            }
            ;
            if (v instanceof Nothing) {
              return Nothing.value;
            }
            ;
            throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
          };
        },
        Apply0: function() {
          return applyMaybe;
        }
      };
    }
  });

  // output/Data.Either/index.js
  var Left, Right, functorEither, either;
  var init_Data16 = __esm({
    "output/Data.Either/index.js"() {
      init_Control7();
      init_Control3();
      init_Data13();
      init_Data8();
      init_Data2();
      init_Data4();
      init_Data_Functor();
      init_Data_Generic();
      init_Data15();
      init_Data12();
      init_Data9();
      init_Data7();
      init_Data14();
      init_Data3();
      Left = /* @__PURE__ */ function() {
        function Left2(value0) {
          this.value0 = value0;
        }
        ;
        Left2.create = function(value0) {
          return new Left2(value0);
        };
        return Left2;
      }();
      Right = /* @__PURE__ */ function() {
        function Right2(value0) {
          this.value0 = value0;
        }
        ;
        Right2.create = function(value0) {
          return new Right2(value0);
        };
        return Right2;
      }();
      functorEither = {
        map: function(f) {
          return function(m) {
            if (m instanceof Left) {
              return new Left(m.value0);
            }
            ;
            if (m instanceof Right) {
              return new Right(f(m.value0));
            }
            ;
            throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
          };
        }
      };
      either = function(v) {
        return function(v1) {
          return function(v2) {
            if (v2 instanceof Left) {
              return v(v2.value0);
            }
            ;
            if (v2 instanceof Right) {
              return v1(v2.value0);
            }
            ;
            throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
          };
        };
      };
    }
  });

  // output/Effect/foreign.js
  var pureE, bindE;
  var init_foreign15 = __esm({
    "output/Effect/foreign.js"() {
      pureE = function(a2) {
        return function() {
          return a2;
        };
      };
      bindE = function(a2) {
        return function(f) {
          return function() {
            return f(a2())();
          };
        };
      };
    }
  });

  // output/Data.EuclideanRing/foreign.js
  var intDegree, intDiv, intMod;
  var init_foreign16 = __esm({
    "output/Data.EuclideanRing/foreign.js"() {
      intDegree = function(x) {
        return Math.min(Math.abs(x), 2147483647);
      };
      intDiv = function(x) {
        return function(y) {
          if (y === 0)
            return 0;
          return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
        };
      };
      intMod = function(x) {
        return function(y) {
          if (y === 0)
            return 0;
          var yy = Math.abs(y);
          return (x % yy + yy) % yy;
        };
      };
    }
  });

  // output/Data.CommutativeRing/index.js
  var commutativeRingInt;
  var init_Data17 = __esm({
    "output/Data.CommutativeRing/index.js"() {
      init_Data11();
      init_Data10();
      init_Data10();
      commutativeRingInt = {
        Ring0: function() {
          return ringInt;
        }
      };
    }
  });

  // output/Data.EuclideanRing/index.js
  var euclideanRingInt, div;
  var init_Data18 = __esm({
    "output/Data.EuclideanRing/index.js"() {
      init_foreign16();
      init_Data17();
      init_Data8();
      init_Data11();
      init_Data10();
      init_Data11();
      init_Data10();
      euclideanRingInt = {
        degree: intDegree,
        div: intDiv,
        mod: intMod,
        CommutativeRing0: function() {
          return commutativeRingInt;
        }
      };
      div = function(dict) {
        return dict.div;
      };
    }
  });

  // output/Data.Monoid/index.js
  var mempty;
  var init_Data19 = __esm({
    "output/Data.Monoid/index.js"() {
      init_Data();
      init_Data18();
      init_Data9();
      init_Data7();
      init_Data5();
      init_Data3();
      init_Record();
      init_Type();
      mempty = function(dict) {
        return dict.mempty;
      };
    }
  });

  // output/Effect/index.js
  var $runtime_lazy, monadEffect, bindEffect, applicativeEffect, $lazy_functorEffect, $lazy_applyEffect, functorEffect;
  var init_Effect = __esm({
    "output/Effect/index.js"() {
      init_foreign15();
      init_Control4();
      init_Control3();
      init_Control6();
      init_Data19();
      init_Data7();
      init_foreign15();
      $runtime_lazy = function(name15, moduleName, init2) {
        var state3 = 0;
        var val;
        return function(lineNumber) {
          if (state3 === 2)
            return val;
          if (state3 === 1)
            throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
          state3 = 1;
          val = init2();
          state3 = 2;
          return val;
        };
      };
      monadEffect = {
        Applicative0: function() {
          return applicativeEffect;
        },
        Bind1: function() {
          return bindEffect;
        }
      };
      bindEffect = {
        bind: bindE,
        Apply0: function() {
          return $lazy_applyEffect(0);
        }
      };
      applicativeEffect = {
        pure: pureE,
        Apply0: function() {
          return $lazy_applyEffect(0);
        }
      };
      $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
        return {
          map: liftA1(applicativeEffect)
        };
      });
      $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
        return {
          apply: ap(monadEffect),
          Functor0: function() {
            return $lazy_functorEffect(0);
          }
        };
      });
      functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);
    }
  });

  // output/Effect.Exception/foreign.js
  function error(msg) {
    return new Error(msg);
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }
  var init_foreign17 = __esm({
    "output/Effect.Exception/foreign.js"() {
    }
  });

  // output/Effect.Exception/index.js
  var $$throw;
  var init_Effect2 = __esm({
    "output/Effect.Exception/index.js"() {
      init_foreign17();
      init_Control4();
      init_Data16();
      init_Data4();
      init_Data15();
      init_Effect();
      init_foreign17();
      $$throw = function($4) {
        return throwException(error($4));
      };
    }
  });

  // output/Control.Monad.Error.Class/index.js
  var throwError, catchError, $$try;
  var init_Control_Monad_Error = __esm({
    "output/Control.Monad.Error.Class/index.js"() {
      init_Control4();
      init_Control5();
      init_Data16();
      init_Data2();
      init_Data4();
      init_Data15();
      init_Data3();
      init_Effect();
      init_Effect2();
      throwError = function(dict) {
        return dict.throwError;
      };
      catchError = function(dict) {
        return dict.catchError;
      };
      $$try = function(dictMonadError) {
        var catchError1 = catchError(dictMonadError);
        var Monad0 = dictMonadError.MonadThrow0().Monad0();
        var map20 = map(Monad0.Bind1().Apply0().Functor0());
        var pure11 = pure(Monad0.Applicative0());
        return function(a2) {
          return catchError1(map20(Right.create)(a2))(function($52) {
            return pure11(Left.create($52));
          });
        };
      };
    }
  });

  // output/Control.Monad.Reader.Class/index.js
  var init_Control_Monad_Reader = __esm({
    "output/Control.Monad.Reader.Class/index.js"() {
      init_Control2();
      init_Control6();
      init_Control();
      init_Data4();
    }
  });

  // output/Data.Identity/index.js
  var Identity, functorIdentity, applyIdentity, bindIdentity, applicativeIdentity, monadIdentity;
  var init_Data20 = __esm({
    "output/Data.Identity/index.js"() {
      init_Data8();
      init_Data_Functor();
      init_Data12();
      init_Data14();
      Identity = function(x) {
        return x;
      };
      functorIdentity = {
        map: function(f) {
          return function(m) {
            return f(m);
          };
        }
      };
      applyIdentity = {
        apply: function(v) {
          return function(v1) {
            return v(v1);
          };
        },
        Functor0: function() {
          return functorIdentity;
        }
      };
      bindIdentity = {
        bind: function(v) {
          return function(f) {
            return f(v);
          };
        },
        Apply0: function() {
          return applyIdentity;
        }
      };
      applicativeIdentity = {
        pure: Identity,
        Apply0: function() {
          return applyIdentity;
        }
      };
      monadIdentity = {
        Applicative0: function() {
          return applicativeIdentity;
        },
        Bind1: function() {
          return bindIdentity;
        }
      };
    }
  });

  // output/Effect.Ref/foreign.js
  var _new, read, modifyImpl, write;
  var init_foreign18 = __esm({
    "output/Effect.Ref/foreign.js"() {
      _new = function(val) {
        return function() {
          return { value: val };
        };
      };
      read = function(ref2) {
        return function() {
          return ref2.value;
        };
      };
      modifyImpl = function(f) {
        return function(ref2) {
          return function() {
            var t = f(ref2.value);
            ref2.value = t.state;
            return t.value;
          };
        };
      };
      write = function(val) {
        return function(ref2) {
          return function() {
            ref2.value = val;
          };
        };
      };
    }
  });

  // output/Effect.Ref/index.js
  var $$void2, $$new, modify$prime, modify, modify_;
  var init_Effect3 = __esm({
    "output/Effect.Ref/index.js"() {
      init_foreign18();
      init_Data4();
      init_Effect();
      init_foreign18();
      $$void2 = /* @__PURE__ */ $$void(functorEffect);
      $$new = _new;
      modify$prime = modifyImpl;
      modify = function(f) {
        return modify$prime(function(s) {
          var s$prime = f(s);
          return {
            state: s$prime,
            value: s$prime
          };
        });
      };
      modify_ = function(f) {
        return function(s) {
          return $$void2(modify(f)(s));
        };
      };
    }
  });

  // output/Control.Monad.Rec.Class/index.js
  var bindFlipped2, map3, Loop, Done, tailRecM, tailRec, monadRecIdentity, monadRecEffect, bifunctorStep;
  var init_Control_Monad_Rec = __esm({
    "output/Control.Monad.Rec.Class/index.js"() {
      init_Control5();
      init_Control6();
      init_Data16();
      init_Data4();
      init_Data20();
      init_Data15();
      init_Data19();
      init_Data7();
      init_Data3();
      init_Effect();
      init_Effect3();
      bindFlipped2 = /* @__PURE__ */ bindFlipped(bindEffect);
      map3 = /* @__PURE__ */ map(functorEffect);
      Loop = /* @__PURE__ */ function() {
        function Loop2(value0) {
          this.value0 = value0;
        }
        ;
        Loop2.create = function(value0) {
          return new Loop2(value0);
        };
        return Loop2;
      }();
      Done = /* @__PURE__ */ function() {
        function Done2(value0) {
          this.value0 = value0;
        }
        ;
        Done2.create = function(value0) {
          return new Done2(value0);
        };
        return Done2;
      }();
      tailRecM = function(dict) {
        return dict.tailRecM;
      };
      tailRec = function(f) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v instanceof Loop) {
              $copy_v = f(v.value0);
              return;
            }
            ;
            if (v instanceof Done) {
              $tco_done = true;
              return v.value0;
            }
            ;
            throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 103, column 3 - line 103, column 25): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
          }
          ;
          return $tco_result;
        };
        return function($85) {
          return go2(f($85));
        };
      };
      monadRecIdentity = {
        tailRecM: function(f) {
          var runIdentity = function(v) {
            return v;
          };
          var $86 = tailRec(function($88) {
            return runIdentity(f($88));
          });
          return function($87) {
            return Identity($86($87));
          };
        },
        Monad0: function() {
          return monadIdentity;
        }
      };
      monadRecEffect = {
        tailRecM: function(f) {
          return function(a2) {
            var fromDone = function(v) {
              if (v instanceof Done) {
                return v.value0;
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 137, column 30 - line 137, column 44): " + [v.constructor.name]);
            };
            return function __do2() {
              var r = bindFlipped2($$new)(f(a2))();
              (function() {
                while (!function __do3() {
                  var v = read(r)();
                  if (v instanceof Loop) {
                    var e = f(v.value0)();
                    write(e)(r)();
                    return false;
                  }
                  ;
                  if (v instanceof Done) {
                    return true;
                  }
                  ;
                  throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 128, column 22 - line 133, column 28): " + [v.constructor.name]);
                }()) {
                }
                ;
                return {};
              })();
              return map3(fromDone)(read(r))();
            };
          };
        },
        Monad0: function() {
          return monadEffect;
        }
      };
      bifunctorStep = {
        bimap: function(v) {
          return function(v1) {
            return function(v2) {
              if (v2 instanceof Loop) {
                return new Loop(v(v2.value0));
              }
              ;
              if (v2 instanceof Done) {
                return new Done(v1(v2.value0));
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 33, column 1 - line 35, column 34): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
            };
          };
        }
      };
    }
  });

  // output/Control.Lazy/index.js
  var init_Control8 = __esm({
    "output/Control.Lazy/index.js"() {
      init_Data3();
    }
  });

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj, boolDisj, boolNot;
  var init_foreign19 = __esm({
    "output/Data.HeytingAlgebra/foreign.js"() {
      boolConj = function(b1) {
        return function(b2) {
          return b1 && b2;
        };
      };
      boolDisj = function(b1) {
        return function(b2) {
          return b1 || b2;
        };
      };
      boolNot = function(b2) {
        return !b2;
      };
    }
  });

  // output/Data.HeytingAlgebra/index.js
  var tt, not, implies, ff, disj, heytingAlgebraBoolean, conj, heytingAlgebraFunction;
  var init_Data21 = __esm({
    "output/Data.HeytingAlgebra/index.js"() {
      init_foreign19();
      init_Data5();
      init_Data3();
      init_Record();
      init_Type();
      tt = function(dict) {
        return dict.tt;
      };
      not = function(dict) {
        return dict.not;
      };
      implies = function(dict) {
        return dict.implies;
      };
      ff = function(dict) {
        return dict.ff;
      };
      disj = function(dict) {
        return dict.disj;
      };
      heytingAlgebraBoolean = {
        ff: false,
        tt: true,
        implies: function(a2) {
          return function(b2) {
            return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a2))(b2);
          };
        },
        conj: boolConj,
        disj: boolDisj,
        not: boolNot
      };
      conj = function(dict) {
        return dict.conj;
      };
      heytingAlgebraFunction = function(dictHeytingAlgebra) {
        var ff1 = ff(dictHeytingAlgebra);
        var tt1 = tt(dictHeytingAlgebra);
        var implies1 = implies(dictHeytingAlgebra);
        var conj1 = conj(dictHeytingAlgebra);
        var disj1 = disj(dictHeytingAlgebra);
        var not1 = not(dictHeytingAlgebra);
        return {
          ff: function(v) {
            return ff1;
          },
          tt: function(v) {
            return tt1;
          },
          implies: function(f) {
            return function(g) {
              return function(a2) {
                return implies1(f(a2))(g(a2));
              };
            };
          },
          conj: function(f) {
            return function(g) {
              return function(a2) {
                return conj1(f(a2))(g(a2));
              };
            };
          },
          disj: function(f) {
            return function(g) {
              return function(a2) {
                return disj1(f(a2))(g(a2));
              };
            };
          },
          not: function(f) {
            return function(a2) {
              return not1(f(a2));
            };
          }
        };
      };
    }
  });

  // output/Data.Tuple/index.js
  var Tuple, snd, functorTuple, fst;
  var init_Data22 = __esm({
    "output/Data.Tuple/index.js"() {
      init_Control8();
      init_Data13();
      init_Data8();
      init_Data_Functor();
      init_Data_Generic();
      init_Data21();
      init_Data19();
      init_Data12();
      init_Data9();
      init_Data11();
      init_Data7();
      init_Data10();
      init_Data14();
      init_Data3();
      Tuple = /* @__PURE__ */ function() {
        function Tuple2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        Tuple2.create = function(value0) {
          return function(value1) {
            return new Tuple2(value0, value1);
          };
        };
        return Tuple2;
      }();
      snd = function(v) {
        return v.value1;
      };
      functorTuple = {
        map: function(f) {
          return function(m) {
            return new Tuple(m.value0, f(m.value1));
          };
        }
      };
      fst = function(v) {
        return v.value0;
      };
    }
  });

  // output/Control.Monad.State.Class/index.js
  var state, put, modify_2, get;
  var init_Control_Monad_State = __esm({
    "output/Control.Monad.State.Class/index.js"() {
      init_Data22();
      init_Data3();
      state = function(dict) {
        return dict.state;
      };
      put = function(dictMonadState) {
        var state1 = state(dictMonadState);
        return function(s) {
          return state1(function(v) {
            return new Tuple(unit, s);
          });
        };
      };
      modify_2 = function(dictMonadState) {
        var state1 = state(dictMonadState);
        return function(f) {
          return state1(function(s) {
            return new Tuple(unit, f(s));
          });
        };
      };
      get = function(dictMonadState) {
        return state(dictMonadState)(function(s) {
          return new Tuple(s, s);
        });
      };
    }
  });

  // output/Control.Monad.Trans.Class/index.js
  var lift;
  var init_Control_Monad_Trans = __esm({
    "output/Control.Monad.Trans.Class/index.js"() {
      lift = function(dict) {
        return dict.lift;
      };
    }
  });

  // output/Control.Monad.Writer.Class/index.js
  var init_Control_Monad_Writer = __esm({
    "output/Control.Monad.Writer.Class/index.js"() {
      init_Control4();
      init_Control5();
      init_Data22();
    }
  });

  // output/Effect.Class/index.js
  var monadEffectEffect, liftEffect;
  var init_Effect4 = __esm({
    "output/Effect.Class/index.js"() {
      init_Control2();
      init_Effect();
      monadEffectEffect = {
        liftEffect: /* @__PURE__ */ identity(categoryFn),
        Monad0: function() {
          return monadEffect;
        }
      };
      liftEffect = function(dict) {
        return dict.liftEffect;
      };
    }
  });

  // output/Control.Monad.Except.Trans/index.js
  var map4, ExceptT, runExceptT, mapExceptT, functorExceptT, monadExceptT, bindExceptT, applyExceptT, applicativeExceptT, monadRecExceptT, monadThrowExceptT, altExceptT, plusExceptT, alternativeExceptT;
  var init_Control_Monad_Except = __esm({
    "output/Control.Monad.Except.Trans/index.js"() {
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control2();
      init_Control6();
      init_Control_Monad_Cont();
      init_Control_Monad_Error();
      init_Control_Monad_Reader();
      init_Control_Monad_Rec();
      init_Control_Monad_State();
      init_Control_Monad_Trans();
      init_Control_Monad_Writer();
      init_Data16();
      init_Data4();
      init_Data19();
      init_Data7();
      init_Data22();
      init_Effect4();
      init_Control_Monad_Error();
      init_Control_Monad_Trans();
      map4 = /* @__PURE__ */ map(functorEither);
      ExceptT = function(x) {
        return x;
      };
      runExceptT = function(v) {
        return v;
      };
      mapExceptT = function(f) {
        return function(v) {
          return f(v);
        };
      };
      functorExceptT = function(dictFunctor) {
        var map110 = map(dictFunctor);
        return {
          map: function(f) {
            return mapExceptT(map110(map4(f)));
          }
        };
      };
      monadExceptT = function(dictMonad) {
        return {
          Applicative0: function() {
            return applicativeExceptT(dictMonad);
          },
          Bind1: function() {
            return bindExceptT(dictMonad);
          }
        };
      };
      bindExceptT = function(dictMonad) {
        var bind7 = bind(dictMonad.Bind1());
        var pure11 = pure(dictMonad.Applicative0());
        return {
          bind: function(v) {
            return function(k) {
              return bind7(v)(either(function($187) {
                return pure11(Left.create($187));
              })(function(a2) {
                var v1 = k(a2);
                return v1;
              }));
            };
          },
          Apply0: function() {
            return applyExceptT(dictMonad);
          }
        };
      };
      applyExceptT = function(dictMonad) {
        var functorExceptT1 = functorExceptT(dictMonad.Bind1().Apply0().Functor0());
        return {
          apply: ap(monadExceptT(dictMonad)),
          Functor0: function() {
            return functorExceptT1;
          }
        };
      };
      applicativeExceptT = function(dictMonad) {
        return {
          pure: function() {
            var $188 = pure(dictMonad.Applicative0());
            return function($189) {
              return ExceptT($188(Right.create($189)));
            };
          }(),
          Apply0: function() {
            return applyExceptT(dictMonad);
          }
        };
      };
      monadRecExceptT = function(dictMonadRec) {
        var tailRecM4 = tailRecM(dictMonadRec);
        var Monad0 = dictMonadRec.Monad0();
        var bind7 = bind(Monad0.Bind1());
        var pure11 = pure(Monad0.Applicative0());
        var monadExceptT1 = monadExceptT(Monad0);
        return {
          tailRecM: function(f) {
            var $193 = tailRecM4(function(a2) {
              var v = f(a2);
              return bind7(v)(function(m$prime) {
                return pure11(function() {
                  if (m$prime instanceof Left) {
                    return new Done(new Left(m$prime.value0));
                  }
                  ;
                  if (m$prime instanceof Right && m$prime.value0 instanceof Loop) {
                    return new Loop(m$prime.value0.value0);
                  }
                  ;
                  if (m$prime instanceof Right && m$prime.value0 instanceof Done) {
                    return new Done(new Right(m$prime.value0.value0));
                  }
                  ;
                  throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 74, column 14 - line 77, column 43): " + [m$prime.constructor.name]);
                }());
              });
            });
            return function($194) {
              return ExceptT($193($194));
            };
          },
          Monad0: function() {
            return monadExceptT1;
          }
        };
      };
      monadThrowExceptT = function(dictMonad) {
        var monadExceptT1 = monadExceptT(dictMonad);
        return {
          throwError: function() {
            var $198 = pure(dictMonad.Applicative0());
            return function($199) {
              return ExceptT($198(Left.create($199)));
            };
          }(),
          Monad0: function() {
            return monadExceptT1;
          }
        };
      };
      altExceptT = function(dictSemigroup) {
        var append7 = append(dictSemigroup);
        return function(dictMonad) {
          var Bind1 = dictMonad.Bind1();
          var bind7 = bind(Bind1);
          var pure11 = pure(dictMonad.Applicative0());
          var functorExceptT1 = functorExceptT(Bind1.Apply0().Functor0());
          return {
            alt: function(v) {
              return function(v1) {
                return bind7(v)(function(rm) {
                  if (rm instanceof Right) {
                    return pure11(new Right(rm.value0));
                  }
                  ;
                  if (rm instanceof Left) {
                    return bind7(v1)(function(rn) {
                      if (rn instanceof Right) {
                        return pure11(new Right(rn.value0));
                      }
                      ;
                      if (rn instanceof Left) {
                        return pure11(new Left(append7(rm.value0)(rn.value0)));
                      }
                      ;
                      throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): " + [rn.constructor.name]);
                    });
                  }
                  ;
                  throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): " + [rm.constructor.name]);
                });
              };
            },
            Functor0: function() {
              return functorExceptT1;
            }
          };
        };
      };
      plusExceptT = function(dictMonoid) {
        var mempty2 = mempty(dictMonoid);
        var altExceptT1 = altExceptT(dictMonoid.Semigroup0());
        return function(dictMonad) {
          var altExceptT2 = altExceptT1(dictMonad);
          return {
            empty: throwError(monadThrowExceptT(dictMonad))(mempty2),
            Alt0: function() {
              return altExceptT2;
            }
          };
        };
      };
      alternativeExceptT = function(dictMonoid) {
        var plusExceptT1 = plusExceptT(dictMonoid);
        return function(dictMonad) {
          var applicativeExceptT1 = applicativeExceptT(dictMonad);
          var plusExceptT2 = plusExceptT1(dictMonad);
          return {
            Applicative0: function() {
              return applicativeExceptT1;
            },
            Plus1: function() {
              return plusExceptT2;
            }
          };
        };
      };
    }
  });

  // output/Control.Plus/index.js
  var plusArray, empty;
  var init_Control9 = __esm({
    "output/Control.Plus/index.js"() {
      init_Control7();
      init_Data4();
      init_Control7();
      init_Data4();
      plusArray = {
        empty: [],
        Alt0: function() {
          return altArray;
        }
      };
      empty = function(dict) {
        return dict.empty;
      };
    }
  });

  // output/Control.Monad.State.Trans/index.js
  var monadTransStateT, lift3, functorStateT, evalStateT, monadStateT, bindStateT, applyStateT, applicativeStateT, monadRecStateT, monadStateStateT, monadThrowStateT, altStateT, plusStateT, alternativeStateT;
  var init_Control_Monad_State2 = __esm({
    "output/Control.Monad.State.Trans/index.js"() {
      init_Control7();
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control6();
      init_Control_Monad_Cont();
      init_Control_Monad_Error();
      init_Control_Monad_Reader();
      init_Control_Monad_Rec();
      init_Control_Monad_State();
      init_Control_Monad_Trans();
      init_Control_Monad_Writer();
      init_Control9();
      init_Data4();
      init_Data19();
      init_Data7();
      init_Data22();
      init_Data3();
      init_Effect4();
      init_Control_Monad_State();
      init_Control_Monad_Trans();
      monadTransStateT = {
        lift: function(dictMonad) {
          var bind7 = bind(dictMonad.Bind1());
          var pure11 = pure(dictMonad.Applicative0());
          return function(m) {
            return function(s) {
              return bind7(m)(function(x) {
                return pure11(new Tuple(x, s));
              });
            };
          };
        }
      };
      lift3 = /* @__PURE__ */ lift(monadTransStateT);
      functorStateT = function(dictFunctor) {
        var map20 = map(dictFunctor);
        return {
          map: function(f) {
            return function(v) {
              return function(s) {
                return map20(function(v1) {
                  return new Tuple(f(v1.value0), v1.value1);
                })(v(s));
              };
            };
          }
        };
      };
      evalStateT = function(dictFunctor) {
        var map20 = map(dictFunctor);
        return function(v) {
          return function(s) {
            return map20(fst)(v(s));
          };
        };
      };
      monadStateT = function(dictMonad) {
        return {
          Applicative0: function() {
            return applicativeStateT(dictMonad);
          },
          Bind1: function() {
            return bindStateT(dictMonad);
          }
        };
      };
      bindStateT = function(dictMonad) {
        var bind7 = bind(dictMonad.Bind1());
        return {
          bind: function(v) {
            return function(f) {
              return function(s) {
                return bind7(v(s))(function(v1) {
                  var v3 = f(v1.value0);
                  return v3(v1.value1);
                });
              };
            };
          },
          Apply0: function() {
            return applyStateT(dictMonad);
          }
        };
      };
      applyStateT = function(dictMonad) {
        var functorStateT1 = functorStateT(dictMonad.Bind1().Apply0().Functor0());
        return {
          apply: ap(monadStateT(dictMonad)),
          Functor0: function() {
            return functorStateT1;
          }
        };
      };
      applicativeStateT = function(dictMonad) {
        var pure11 = pure(dictMonad.Applicative0());
        return {
          pure: function(a2) {
            return function(s) {
              return pure11(new Tuple(a2, s));
            };
          },
          Apply0: function() {
            return applyStateT(dictMonad);
          }
        };
      };
      monadRecStateT = function(dictMonadRec) {
        var Monad0 = dictMonadRec.Monad0();
        var bind7 = bind(Monad0.Bind1());
        var pure11 = pure(Monad0.Applicative0());
        var tailRecM4 = tailRecM(dictMonadRec);
        var monadStateT1 = monadStateT(Monad0);
        return {
          tailRecM: function(f) {
            return function(a2) {
              var f$prime = function(v) {
                var v1 = f(v.value0);
                return bind7(v1(v.value1))(function(v2) {
                  return pure11(function() {
                    if (v2.value0 instanceof Loop) {
                      return new Loop(new Tuple(v2.value0.value0, v2.value1));
                    }
                    ;
                    if (v2.value0 instanceof Done) {
                      return new Done(new Tuple(v2.value0.value0, v2.value1));
                    }
                    ;
                    throw new Error("Failed pattern match at Control.Monad.State.Trans (line 87, column 16 - line 89, column 40): " + [v2.value0.constructor.name]);
                  }());
                });
              };
              return function(s) {
                return tailRecM4(f$prime)(new Tuple(a2, s));
              };
            };
          },
          Monad0: function() {
            return monadStateT1;
          }
        };
      };
      monadStateStateT = function(dictMonad) {
        var pure11 = pure(dictMonad.Applicative0());
        var monadStateT1 = monadStateT(dictMonad);
        return {
          state: function(f) {
            return function($200) {
              return pure11(f($200));
            };
          },
          Monad0: function() {
            return monadStateT1;
          }
        };
      };
      monadThrowStateT = function(dictMonadThrow) {
        var Monad0 = dictMonadThrow.Monad0();
        var lift1 = lift3(Monad0);
        var throwError4 = throwError(dictMonadThrow);
        var monadStateT1 = monadStateT(Monad0);
        return {
          throwError: function(e) {
            return lift1(throwError4(e));
          },
          Monad0: function() {
            return monadStateT1;
          }
        };
      };
      altStateT = function(dictMonad) {
        return function(dictAlt) {
          var alt6 = alt(dictAlt);
          var functorStateT1 = functorStateT(dictAlt.Functor0());
          return {
            alt: function(v) {
              return function(v1) {
                return function(s) {
                  return alt6(v(s))(v1(s));
                };
              };
            },
            Functor0: function() {
              return functorStateT1;
            }
          };
        };
      };
      plusStateT = function(dictMonad) {
        var altStateT1 = altStateT(dictMonad);
        return function(dictPlus) {
          var empty8 = empty(dictPlus);
          var altStateT2 = altStateT1(dictPlus.Alt0());
          return {
            empty: function(v) {
              return empty8;
            },
            Alt0: function() {
              return altStateT2;
            }
          };
        };
      };
      alternativeStateT = function(dictMonad) {
        var applicativeStateT1 = applicativeStateT(dictMonad);
        var plusStateT1 = plusStateT(dictMonad);
        return function(dictAlternative) {
          var plusStateT2 = plusStateT1(dictAlternative.Plus1());
          return {
            Applicative0: function() {
              return applicativeStateT1;
            },
            Plus1: function() {
              return plusStateT2;
            }
          };
        };
      };
    }
  });

  // output/Data.MediaType/index.js
  var init_Data23 = __esm({
    "output/Data.MediaType/index.js"() {
      init_Data12();
      init_Data14();
    }
  });

  // output/Data.String.Common/foreign.js
  var joinWith;
  var init_foreign20 = __esm({
    "output/Data.String.Common/foreign.js"() {
      joinWith = function(s) {
        return function(xs) {
          return xs.join(s);
        };
      };
    }
  });

  // output/Data.String.Common/index.js
  var init_Data_String = __esm({
    "output/Data.String.Common/index.js"() {
      init_foreign20();
      init_Data9();
      init_foreign20();
    }
  });

  // output/DOM.HTML.Indexed.InputAcceptType/index.js
  var map5, AcceptMediaType, AcceptFileExtension, renderInputAcceptTypeAtom, renderInputAcceptType;
  var init_DOM_HTML_Indexed = __esm({
    "output/DOM.HTML.Indexed.InputAcceptType/index.js"() {
      init_Data8();
      init_Data4();
      init_Data23();
      init_Data12();
      init_Data9();
      init_Data7();
      init_Data_String();
      map5 = /* @__PURE__ */ map(functorArray);
      AcceptMediaType = /* @__PURE__ */ function() {
        function AcceptMediaType2(value0) {
          this.value0 = value0;
        }
        ;
        AcceptMediaType2.create = function(value0) {
          return new AcceptMediaType2(value0);
        };
        return AcceptMediaType2;
      }();
      AcceptFileExtension = /* @__PURE__ */ function() {
        function AcceptFileExtension2(value0) {
          this.value0 = value0;
        }
        ;
        AcceptFileExtension2.create = function(value0) {
          return new AcceptFileExtension2(value0);
        };
        return AcceptFileExtension2;
      }();
      renderInputAcceptTypeAtom = function(v) {
        if (v instanceof AcceptMediaType) {
          return v.value0;
        }
        ;
        if (v instanceof AcceptFileExtension) {
          return v.value0;
        }
        ;
        throw new Error("Failed pattern match at DOM.HTML.Indexed.InputAcceptType (line 34, column 29 - line 36, column 33): " + [v.constructor.name]);
      };
      renderInputAcceptType = function(v) {
        return joinWith(",")(map5(renderInputAcceptTypeAtom)(v));
      };
    }
  });

  // output/DOM.HTML.Indexed.InputType/index.js
  var InputButton, InputCheckbox, InputColor, InputDate, InputDatetimeLocal, InputEmail, InputFile, InputHidden, InputImage, InputMonth, InputNumber, InputPassword, InputRadio, InputRange, InputReset, InputSearch, InputSubmit, InputTel, InputText, InputTime, InputUrl, InputWeek, renderInputType;
  var init_DOM_HTML_Indexed2 = __esm({
    "output/DOM.HTML.Indexed.InputType/index.js"() {
      init_Data9();
      InputButton = /* @__PURE__ */ function() {
        function InputButton2() {
        }
        ;
        InputButton2.value = new InputButton2();
        return InputButton2;
      }();
      InputCheckbox = /* @__PURE__ */ function() {
        function InputCheckbox2() {
        }
        ;
        InputCheckbox2.value = new InputCheckbox2();
        return InputCheckbox2;
      }();
      InputColor = /* @__PURE__ */ function() {
        function InputColor2() {
        }
        ;
        InputColor2.value = new InputColor2();
        return InputColor2;
      }();
      InputDate = /* @__PURE__ */ function() {
        function InputDate2() {
        }
        ;
        InputDate2.value = new InputDate2();
        return InputDate2;
      }();
      InputDatetimeLocal = /* @__PURE__ */ function() {
        function InputDatetimeLocal2() {
        }
        ;
        InputDatetimeLocal2.value = new InputDatetimeLocal2();
        return InputDatetimeLocal2;
      }();
      InputEmail = /* @__PURE__ */ function() {
        function InputEmail2() {
        }
        ;
        InputEmail2.value = new InputEmail2();
        return InputEmail2;
      }();
      InputFile = /* @__PURE__ */ function() {
        function InputFile2() {
        }
        ;
        InputFile2.value = new InputFile2();
        return InputFile2;
      }();
      InputHidden = /* @__PURE__ */ function() {
        function InputHidden2() {
        }
        ;
        InputHidden2.value = new InputHidden2();
        return InputHidden2;
      }();
      InputImage = /* @__PURE__ */ function() {
        function InputImage2() {
        }
        ;
        InputImage2.value = new InputImage2();
        return InputImage2;
      }();
      InputMonth = /* @__PURE__ */ function() {
        function InputMonth2() {
        }
        ;
        InputMonth2.value = new InputMonth2();
        return InputMonth2;
      }();
      InputNumber = /* @__PURE__ */ function() {
        function InputNumber2() {
        }
        ;
        InputNumber2.value = new InputNumber2();
        return InputNumber2;
      }();
      InputPassword = /* @__PURE__ */ function() {
        function InputPassword2() {
        }
        ;
        InputPassword2.value = new InputPassword2();
        return InputPassword2;
      }();
      InputRadio = /* @__PURE__ */ function() {
        function InputRadio2() {
        }
        ;
        InputRadio2.value = new InputRadio2();
        return InputRadio2;
      }();
      InputRange = /* @__PURE__ */ function() {
        function InputRange2() {
        }
        ;
        InputRange2.value = new InputRange2();
        return InputRange2;
      }();
      InputReset = /* @__PURE__ */ function() {
        function InputReset2() {
        }
        ;
        InputReset2.value = new InputReset2();
        return InputReset2;
      }();
      InputSearch = /* @__PURE__ */ function() {
        function InputSearch2() {
        }
        ;
        InputSearch2.value = new InputSearch2();
        return InputSearch2;
      }();
      InputSubmit = /* @__PURE__ */ function() {
        function InputSubmit2() {
        }
        ;
        InputSubmit2.value = new InputSubmit2();
        return InputSubmit2;
      }();
      InputTel = /* @__PURE__ */ function() {
        function InputTel2() {
        }
        ;
        InputTel2.value = new InputTel2();
        return InputTel2;
      }();
      InputText = /* @__PURE__ */ function() {
        function InputText2() {
        }
        ;
        InputText2.value = new InputText2();
        return InputText2;
      }();
      InputTime = /* @__PURE__ */ function() {
        function InputTime2() {
        }
        ;
        InputTime2.value = new InputTime2();
        return InputTime2;
      }();
      InputUrl = /* @__PURE__ */ function() {
        function InputUrl2() {
        }
        ;
        InputUrl2.value = new InputUrl2();
        return InputUrl2;
      }();
      InputWeek = /* @__PURE__ */ function() {
        function InputWeek2() {
        }
        ;
        InputWeek2.value = new InputWeek2();
        return InputWeek2;
      }();
      renderInputType = function(v) {
        if (v instanceof InputButton) {
          return "button";
        }
        ;
        if (v instanceof InputCheckbox) {
          return "checkbox";
        }
        ;
        if (v instanceof InputColor) {
          return "color";
        }
        ;
        if (v instanceof InputDate) {
          return "date";
        }
        ;
        if (v instanceof InputDatetimeLocal) {
          return "datetime-local";
        }
        ;
        if (v instanceof InputEmail) {
          return "email";
        }
        ;
        if (v instanceof InputFile) {
          return "file";
        }
        ;
        if (v instanceof InputHidden) {
          return "hidden";
        }
        ;
        if (v instanceof InputImage) {
          return "image";
        }
        ;
        if (v instanceof InputMonth) {
          return "month";
        }
        ;
        if (v instanceof InputNumber) {
          return "number";
        }
        ;
        if (v instanceof InputPassword) {
          return "password";
        }
        ;
        if (v instanceof InputRadio) {
          return "radio";
        }
        ;
        if (v instanceof InputRange) {
          return "range";
        }
        ;
        if (v instanceof InputReset) {
          return "reset";
        }
        ;
        if (v instanceof InputSearch) {
          return "search";
        }
        ;
        if (v instanceof InputSubmit) {
          return "submit";
        }
        ;
        if (v instanceof InputTel) {
          return "tel";
        }
        ;
        if (v instanceof InputText) {
          return "text";
        }
        ;
        if (v instanceof InputTime) {
          return "time";
        }
        ;
        if (v instanceof InputUrl) {
          return "url";
        }
        ;
        if (v instanceof InputWeek) {
          return "week";
        }
        ;
        throw new Error("Failed pattern match at DOM.HTML.Indexed.InputType (line 33, column 19 - line 55, column 22): " + [v.constructor.name]);
      };
    }
  });

  // output/Data.Array/foreign.js
  var replicateFill, replicatePolyfill, replicateImpl, fromFoldableImpl, length, indexImpl, findIndexImpl, _deleteAt, sortByImpl;
  var init_foreign21 = __esm({
    "output/Data.Array/foreign.js"() {
      replicateFill = function(count, value12) {
        if (count < 1) {
          return [];
        }
        var result = new Array(count);
        return result.fill(value12);
      };
      replicatePolyfill = function(count, value12) {
        var result = [];
        var n = 0;
        for (var i2 = 0; i2 < count; i2++) {
          result[n++] = value12;
        }
        return result;
      };
      replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
      fromFoldableImpl = function() {
        function Cons3(head3, tail) {
          this.head = head3;
          this.tail = tail;
        }
        var emptyList = {};
        function curryCons(head3) {
          return function(tail) {
            return new Cons3(head3, tail);
          };
        }
        function listToArray(list) {
          var result = [];
          var count = 0;
          var xs = list;
          while (xs !== emptyList) {
            result[count++] = xs.head;
            xs = xs.tail;
          }
          return result;
        }
        return function(foldr5, xs) {
          return listToArray(foldr5(curryCons)(emptyList)(xs));
        };
      }();
      length = function(xs) {
        return xs.length;
      };
      indexImpl = function(just, nothing, xs, i2) {
        return i2 < 0 || i2 >= xs.length ? nothing : just(xs[i2]);
      };
      findIndexImpl = function(just, nothing, f, xs) {
        for (var i2 = 0, l = xs.length; i2 < l; i2++) {
          if (f(xs[i2]))
            return just(i2);
        }
        return nothing;
      };
      _deleteAt = function(just, nothing, i2, l) {
        if (i2 < 0 || i2 >= l.length)
          return nothing;
        var l1 = l.slice();
        l1.splice(i2, 1);
        return just(l1);
      };
      sortByImpl = function() {
        function mergeFromTo(compare2, fromOrdering, xs1, xs2, from3, to) {
          var mid;
          var i2;
          var j;
          var k;
          var x;
          var y;
          var c;
          mid = from3 + (to - from3 >> 1);
          if (mid - from3 > 1)
            mergeFromTo(compare2, fromOrdering, xs2, xs1, from3, mid);
          if (to - mid > 1)
            mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
          i2 = from3;
          j = mid;
          k = from3;
          while (i2 < mid && j < to) {
            x = xs2[i2];
            y = xs2[j];
            c = fromOrdering(compare2(x)(y));
            if (c > 0) {
              xs1[k++] = y;
              ++j;
            } else {
              xs1[k++] = x;
              ++i2;
            }
          }
          while (i2 < mid) {
            xs1[k++] = xs2[i2++];
          }
          while (j < to) {
            xs1[k++] = xs2[j++];
          }
        }
        return function(compare2, fromOrdering, xs) {
          var out;
          if (xs.length < 2)
            return xs;
          out = xs.slice(0);
          mergeFromTo(compare2, fromOrdering, out, xs.slice(0), 0, xs.length);
          return out;
        };
      }();
    }
  });

  // output/Control.Monad.ST.Internal/foreign.js
  var init_foreign22 = __esm({
    "output/Control.Monad.ST.Internal/foreign.js"() {
    }
  });

  // output/Control.Monad.ST.Internal/index.js
  var init_Control_Monad_ST = __esm({
    "output/Control.Monad.ST.Internal/index.js"() {
      init_foreign22();
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control6();
      init_Control_Monad_Rec();
      init_Data4();
      init_Data19();
      init_Data7();
      init_Data3();
      init_foreign22();
    }
  });

  // output/Data.Array.ST/foreign.js
  function unsafeFreezeThawImpl(xs) {
    return xs;
  }
  function copyImpl(xs) {
    return xs.slice();
  }
  var pushAllImpl, unsafeFreezeImpl, thawImpl, sortByImpl2;
  var init_foreign23 = __esm({
    "output/Data.Array.ST/foreign.js"() {
      pushAllImpl = function(as, xs) {
        return xs.push.apply(xs, as);
      };
      unsafeFreezeImpl = unsafeFreezeThawImpl;
      thawImpl = copyImpl;
      sortByImpl2 = function() {
        function mergeFromTo(compare2, fromOrdering, xs1, xs2, from3, to) {
          var mid;
          var i2;
          var j;
          var k;
          var x;
          var y;
          var c;
          mid = from3 + (to - from3 >> 1);
          if (mid - from3 > 1)
            mergeFromTo(compare2, fromOrdering, xs2, xs1, from3, mid);
          if (to - mid > 1)
            mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
          i2 = from3;
          j = mid;
          k = from3;
          while (i2 < mid && j < to) {
            x = xs2[i2];
            y = xs2[j];
            c = fromOrdering(compare2(x)(y));
            if (c > 0) {
              xs1[k++] = y;
              ++j;
            } else {
              xs1[k++] = x;
              ++i2;
            }
          }
          while (i2 < mid) {
            xs1[k++] = xs2[i2++];
          }
          while (j < to) {
            xs1[k++] = xs2[j++];
          }
        }
        return function(compare2, fromOrdering, xs) {
          if (xs.length < 2)
            return xs;
          mergeFromTo(compare2, fromOrdering, xs, xs.slice(0), 0, xs.length);
          return xs;
        };
      }();
    }
  });

  // output/Control.Monad.ST.Uncurried/foreign.js
  var runSTFn1, runSTFn2;
  var init_foreign24 = __esm({
    "output/Control.Monad.ST.Uncurried/foreign.js"() {
      runSTFn1 = function runSTFn12(fn) {
        return function(a2) {
          return function() {
            return fn(a2);
          };
        };
      };
      runSTFn2 = function runSTFn22(fn) {
        return function(a2) {
          return function(b2) {
            return function() {
              return fn(a2, b2);
            };
          };
        };
      };
    }
  });

  // output/Control.Monad.ST.Uncurried/index.js
  var init_Control_Monad_ST2 = __esm({
    "output/Control.Monad.ST.Uncurried/index.js"() {
      init_foreign24();
      init_foreign24();
    }
  });

  // output/Data.Array.ST/index.js
  var unsafeFreeze, thaw, withArray, push;
  var init_Data_Array = __esm({
    "output/Data.Array.ST/index.js"() {
      init_foreign23();
      init_Control5();
      init_Control_Monad_ST();
      init_Control_Monad_ST2();
      init_Data15();
      init_Data12();
      init_Data9();
      init_foreign23();
      unsafeFreeze = /* @__PURE__ */ runSTFn1(unsafeFreezeImpl);
      thaw = /* @__PURE__ */ runSTFn1(thawImpl);
      withArray = function(f) {
        return function(xs) {
          return function __do2() {
            var result = thaw(xs)();
            f(result)();
            return unsafeFreeze(result)();
          };
        };
      };
      push = function(a2) {
        return runSTFn2(pushAllImpl)([a2]);
      };
    }
  });

  // output/Data.Array.ST.Iterator/index.js
  var init_Data_Array_ST = __esm({
    "output/Data.Array.ST.Iterator/index.js"() {
      init_Control_Monad_ST();
      init_Data_Array();
      init_Data2();
      init_Data4();
      init_Data21();
      init_Data15();
    }
  });

  // output/Data.Foldable/foreign.js
  var foldrArray, foldlArray;
  var init_foreign25 = __esm({
    "output/Data.Foldable/foreign.js"() {
      foldrArray = function(f) {
        return function(init2) {
          return function(xs) {
            var acc = init2;
            var len = xs.length;
            for (var i2 = len - 1; i2 >= 0; i2--) {
              acc = f(xs[i2])(acc);
            }
            return acc;
          };
        };
      };
      foldlArray = function(f) {
        return function(init2) {
          return function(xs) {
            var acc = init2;
            var len = xs.length;
            for (var i2 = 0; i2 < len; i2++) {
              acc = f(acc)(xs[i2]);
            }
            return acc;
          };
        };
      };
    }
  });

  // output/Control.Extend/foreign.js
  var init_foreign26 = __esm({
    "output/Control.Extend/foreign.js"() {
    }
  });

  // output/Control.Extend/index.js
  var init_Control10 = __esm({
    "output/Control.Extend/index.js"() {
      init_foreign26();
      init_Control2();
      init_Data4();
      init_Data7();
      init_Data4();
    }
  });

  // output/Control.Comonad/index.js
  var init_Control11 = __esm({
    "output/Control.Comonad/index.js"() {
      init_Control10();
      init_Data4();
      init_Control10();
      init_Data4();
    }
  });

  // output/Data.Bifunctor/index.js
  var bimap;
  var init_Data24 = __esm({
    "output/Data.Bifunctor/index.js"() {
      init_Control2();
      init_Data16();
      init_Data22();
      bimap = function(dict) {
        return dict.bimap;
      };
    }
  });

  // output/Data.Functor.Coproduct/index.js
  var init_Data_Functor2 = __esm({
    "output/Data.Functor.Coproduct/index.js"() {
      init_Control11();
      init_Control10();
      init_Data24();
      init_Data16();
      init_Data8();
      init_Data4();
      init_Data12();
      init_Data9();
      init_Data14();
    }
  });

  // output/Data.Maybe.First/index.js
  var init_Data_Maybe = __esm({
    "output/Data.Maybe.First/index.js"() {
      init_Data15();
      init_Data19();
      init_Data7();
      init_Data14();
    }
  });

  // output/Data.Monoid.Conj/index.js
  var init_Data_Monoid = __esm({
    "output/Data.Monoid.Conj/index.js"() {
      init_Data8();
      init_Data21();
      init_Data12();
      init_Data14();
    }
  });

  // output/Data.Monoid.Disj/index.js
  var init_Data_Monoid2 = __esm({
    "output/Data.Monoid.Disj/index.js"() {
      init_Data8();
      init_Data21();
      init_Data12();
      init_Data14();
    }
  });

  // output/Data.Monoid.Dual/index.js
  var init_Data_Monoid3 = __esm({
    "output/Data.Monoid.Dual/index.js"() {
      init_Data8();
      init_Data19();
      init_Data12();
      init_Data7();
      init_Data14();
    }
  });

  // output/Data.Monoid.Endo/index.js
  var init_Data_Monoid4 = __esm({
    "output/Data.Monoid.Endo/index.js"() {
      init_Control2();
      init_Control();
      init_Data14();
    }
  });

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2;
  var init_foreign27 = __esm({
    "output/Unsafe.Coerce/foreign.js"() {
      unsafeCoerce2 = function(x) {
        return x;
      };
    }
  });

  // output/Unsafe.Coerce/index.js
  var init_Unsafe = __esm({
    "output/Unsafe.Coerce/index.js"() {
      init_foreign27();
      init_foreign27();
    }
  });

  // output/Safe.Coerce/index.js
  var coerce;
  var init_Safe = __esm({
    "output/Safe.Coerce/index.js"() {
      init_Unsafe();
      coerce = function() {
        return unsafeCoerce2;
      };
    }
  });

  // output/Data.Newtype/index.js
  var coerce2, unwrap;
  var init_Data25 = __esm({
    "output/Data.Newtype/index.js"() {
      init_Safe();
      coerce2 = /* @__PURE__ */ coerce();
      unwrap = function() {
        return coerce2;
      };
    }
  });

  // output/Data.Foldable/index.js
  var foldr, traverse_, for_, foldl, foldableMaybe, foldMapDefaultR, foldableArray, foldMap;
  var init_Data26 = __esm({
    "output/Data.Foldable/index.js"() {
      init_foreign25();
      init_Control7();
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control2();
      init_Control9();
      init_Data16();
      init_Data8();
      init_Data2();
      init_Data_Functor2();
      init_Data21();
      init_Data15();
      init_Data_Maybe();
      init_Data19();
      init_Data_Monoid();
      init_Data_Monoid2();
      init_Data_Monoid3();
      init_Data_Monoid4();
      init_Data25();
      init_Data12();
      init_Data9();
      init_Data7();
      init_Data10();
      init_Data3();
      foldr = function(dict) {
        return dict.foldr;
      };
      traverse_ = function(dictApplicative) {
        var applySecond2 = applySecond(dictApplicative.Apply0());
        var pure11 = pure(dictApplicative);
        return function(dictFoldable) {
          var foldr22 = foldr(dictFoldable);
          return function(f) {
            return foldr22(function($454) {
              return applySecond2(f($454));
            })(pure11(unit));
          };
        };
      };
      for_ = function(dictApplicative) {
        var traverse_14 = traverse_(dictApplicative);
        return function(dictFoldable) {
          return flip(traverse_14(dictFoldable));
        };
      };
      foldl = function(dict) {
        return dict.foldl;
      };
      foldableMaybe = {
        foldr: function(v) {
          return function(v1) {
            return function(v2) {
              if (v2 instanceof Nothing) {
                return v1;
              }
              ;
              if (v2 instanceof Just) {
                return v(v2.value0)(v1);
              }
              ;
              throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
            };
          };
        },
        foldl: function(v) {
          return function(v1) {
            return function(v2) {
              if (v2 instanceof Nothing) {
                return v1;
              }
              ;
              if (v2 instanceof Just) {
                return v(v1)(v2.value0);
              }
              ;
              throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
            };
          };
        },
        foldMap: function(dictMonoid) {
          var mempty2 = mempty(dictMonoid);
          return function(v) {
            return function(v1) {
              if (v1 instanceof Nothing) {
                return mempty2;
              }
              ;
              if (v1 instanceof Just) {
                return v(v1.value0);
              }
              ;
              throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name]);
            };
          };
        }
      };
      foldMapDefaultR = function(dictFoldable) {
        var foldr22 = foldr(dictFoldable);
        return function(dictMonoid) {
          var append7 = append(dictMonoid.Semigroup0());
          var mempty2 = mempty(dictMonoid);
          return function(f) {
            return foldr22(function(x) {
              return function(acc) {
                return append7(f(x))(acc);
              };
            })(mempty2);
          };
        };
      };
      foldableArray = {
        foldr: foldrArray,
        foldl: foldlArray,
        foldMap: function(dictMonoid) {
          return foldMapDefaultR(foldableArray)(dictMonoid);
        }
      };
      foldMap = function(dict) {
        return dict.foldMap;
      };
    }
  });

  // output/Data.Function.Uncurried/foreign.js
  var runFn2, runFn4;
  var init_foreign28 = __esm({
    "output/Data.Function.Uncurried/foreign.js"() {
      runFn2 = function(fn) {
        return function(a2) {
          return function(b2) {
            return fn(a2, b2);
          };
        };
      };
      runFn4 = function(fn) {
        return function(a2) {
          return function(b2) {
            return function(c) {
              return function(d) {
                return fn(a2, b2, c, d);
              };
            };
          };
        };
      };
    }
  });

  // output/Data.Function.Uncurried/index.js
  var init_Data_Function = __esm({
    "output/Data.Function.Uncurried/index.js"() {
      init_foreign28();
      init_foreign28();
    }
  });

  // output/Data.FunctorWithIndex/foreign.js
  var init_foreign29 = __esm({
    "output/Data.FunctorWithIndex/foreign.js"() {
    }
  });

  // output/Data.Const/index.js
  var init_Data27 = __esm({
    "output/Data.Const/index.js"() {
      init_Data8();
      init_Data_Functor();
      init_Data19();
      init_Data12();
      init_Data7();
      init_Data14();
    }
  });

  // output/Data.Functor.App/index.js
  var init_Data_Functor3 = __esm({
    "output/Data.Functor.App/index.js"() {
      init_Control4();
      init_Control3();
      init_Data8();
      init_Data19();
      init_Data12();
      init_Data7();
      init_Data14();
      init_Unsafe();
    }
  });

  // output/Data.Functor.Compose/index.js
  var init_Data_Functor4 = __esm({
    "output/Data.Functor.Compose/index.js"() {
      init_Control7();
      init_Control4();
      init_Control3();
      init_Control9();
      init_Data8();
      init_Data4();
      init_Data_Functor3();
      init_Data12();
      init_Data14();
    }
  });

  // output/Data.Functor.Product/index.js
  var init_Data_Functor5 = __esm({
    "output/Data.Functor.Product/index.js"() {
      init_Control4();
      init_Control3();
      init_Control5();
      init_Data24();
      init_Data8();
      init_Data4();
      init_Data25();
      init_Data12();
      init_Data9();
      init_Data14();
      init_Data22();
    }
  });

  // output/Data.Maybe.Last/index.js
  var init_Data_Maybe2 = __esm({
    "output/Data.Maybe.Last/index.js"() {
      init_Data15();
      init_Data19();
      init_Data7();
      init_Data14();
    }
  });

  // output/Data.Monoid.Additive/index.js
  var init_Data_Monoid5 = __esm({
    "output/Data.Monoid.Additive/index.js"() {
      init_Data8();
      init_Data12();
      init_Data10();
      init_Data14();
    }
  });

  // output/Data.Monoid.Multiplicative/index.js
  var init_Data_Monoid6 = __esm({
    "output/Data.Monoid.Multiplicative/index.js"() {
      init_Data8();
      init_Data12();
      init_Data10();
      init_Data14();
    }
  });

  // output/Data.FunctorWithIndex/index.js
  var init_Data28 = __esm({
    "output/Data.FunctorWithIndex/index.js"() {
      init_foreign29();
      init_Data24();
      init_Data27();
      init_Data16();
      init_Data2();
      init_Data4();
      init_Data_Functor3();
      init_Data_Functor4();
      init_Data_Functor2();
      init_Data_Functor5();
      init_Data20();
      init_Data15();
      init_Data_Maybe();
      init_Data_Maybe2();
      init_Data_Monoid5();
      init_Data_Monoid();
      init_Data_Monoid2();
      init_Data_Monoid3();
      init_Data_Monoid6();
      init_Data22();
      init_Data3();
    }
  });

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl;
  var init_foreign30 = __esm({
    "output/Data.Traversable/foreign.js"() {
      traverseArrayImpl = function() {
        function array1(a2) {
          return [a2];
        }
        function array2(a2) {
          return function(b2) {
            return [a2, b2];
          };
        }
        function array3(a2) {
          return function(b2) {
            return function(c) {
              return [a2, b2, c];
            };
          };
        }
        function concat2(xs) {
          return function(ys) {
            return xs.concat(ys);
          };
        }
        return function(apply2) {
          return function(map20) {
            return function(pure11) {
              return function(f) {
                return function(array) {
                  function go2(bot, top2) {
                    switch (top2 - bot) {
                      case 0:
                        return pure11([]);
                      case 1:
                        return map20(array1)(f(array[bot]));
                      case 2:
                        return apply2(map20(array2)(f(array[bot])))(f(array[bot + 1]));
                      case 3:
                        return apply2(apply2(map20(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                      default:
                        var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                        return apply2(map20(concat2)(go2(bot, pivot)))(go2(pivot, top2));
                    }
                  }
                  return go2(0, array.length);
                };
              };
            };
          };
        };
      }();
    }
  });

  // output/Data.Traversable.Accum.Internal/index.js
  var init_Data_Traversable_Accum = __esm({
    "output/Data.Traversable.Accum.Internal/index.js"() {
    }
  });

  // output/Data.Traversable/index.js
  var identity4, traverse, sequenceDefault, traversableArray, sequence;
  var init_Data29 = __esm({
    "output/Data.Traversable/index.js"() {
      init_foreign30();
      init_Control4();
      init_Control3();
      init_Control2();
      init_Data27();
      init_Data16();
      init_Data26();
      init_Data4();
      init_Data_Functor3();
      init_Data_Functor4();
      init_Data_Functor2();
      init_Data_Functor5();
      init_Data20();
      init_Data15();
      init_Data_Maybe();
      init_Data_Maybe2();
      init_Data_Monoid5();
      init_Data_Monoid();
      init_Data_Monoid2();
      init_Data_Monoid3();
      init_Data_Monoid6();
      init_Data_Traversable_Accum();
      init_Data22();
      init_Data26();
      identity4 = /* @__PURE__ */ identity(categoryFn);
      traverse = function(dict) {
        return dict.traverse;
      };
      sequenceDefault = function(dictTraversable) {
        var traverse2 = traverse(dictTraversable);
        return function(dictApplicative) {
          return traverse2(dictApplicative)(identity4);
        };
      };
      traversableArray = {
        traverse: function(dictApplicative) {
          var Apply0 = dictApplicative.Apply0();
          return traverseArrayImpl(apply(Apply0))(map(Apply0.Functor0()))(pure(dictApplicative));
        },
        sequence: function(dictApplicative) {
          return sequenceDefault(traversableArray)(dictApplicative);
        },
        Functor0: function() {
          return functorArray;
        },
        Foldable1: function() {
          return foldableArray;
        }
      };
      sequence = function(dict) {
        return dict.sequence;
      };
    }
  });

  // output/Data.Unfoldable/foreign.js
  var init_foreign31 = __esm({
    "output/Data.Unfoldable/foreign.js"() {
    }
  });

  // output/Data.Unfoldable1/foreign.js
  var init_foreign32 = __esm({
    "output/Data.Unfoldable1/foreign.js"() {
    }
  });

  // output/Data.Ord.Max/index.js
  var init_Data_Ord = __esm({
    "output/Data.Ord.Max/index.js"() {
      init_Data13();
      init_Data12();
      init_Data14();
    }
  });

  // output/Data.Ord.Min/index.js
  var init_Data_Ord2 = __esm({
    "output/Data.Ord.Min/index.js"() {
      init_Data13();
      init_Data12();
      init_Data14();
    }
  });

  // output/Data.Semigroup.Foldable/index.js
  var init_Data_Semigroup = __esm({
    "output/Data.Semigroup.Foldable/index.js"() {
      init_Control3();
      init_Control2();
      init_Data8();
      init_Data26();
      init_Data2();
      init_Data4();
      init_Data_Monoid3();
      init_Data25();
      init_Data_Ord();
      init_Data_Ord2();
      init_Data9();
      init_Data7();
      init_Data3();
    }
  });

  // output/Data.Semigroup.Traversable/index.js
  var init_Data_Semigroup2 = __esm({
    "output/Data.Semigroup.Traversable/index.js"() {
      init_Control2();
      init_Data4();
      init_Data20();
      init_Data_Monoid3();
      init_Data_Monoid6();
      init_Data_Semigroup();
      init_Data29();
      init_Data22();
    }
  });

  // output/Data.Unfoldable1/index.js
  var init_Data30 = __esm({
    "output/Data.Unfoldable1/index.js"() {
      init_foreign32();
      init_Data();
      init_Data15();
      init_Data_Semigroup2();
      init_Data22();
    }
  });

  // output/Data.Unfoldable/index.js
  var init_Data31 = __esm({
    "output/Data.Unfoldable/index.js"() {
      init_foreign31();
      init_Data2();
      init_Data4();
      init_Data15();
      init_Data29();
      init_Data22();
      init_Data30();
      init_Data3();
      init_Data30();
    }
  });

  // output/Data.Array/index.js
  var fromJust2, snoc, singleton2, replicate, index, fromFoldable, foldr2, findIndex, deleteAt, deleteBy, concatMap, mapMaybe;
  var init_Data32 = __esm({
    "output/Data.Array/index.js"() {
      init_foreign21();
      init_Control7();
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control2();
      init_Control8();
      init_Control_Monad_Rec();
      init_Control_Monad_ST();
      init_Data_Array();
      init_Data_Array_ST();
      init_Data();
      init_Data8();
      init_Data26();
      init_Data2();
      init_Data_Function();
      init_Data4();
      init_Data28();
      init_Data15();
      init_Data12();
      init_Data9();
      init_Data7();
      init_Data29();
      init_Data22();
      init_Data31();
      init_foreign21();
      fromJust2 = /* @__PURE__ */ fromJust();
      snoc = function(xs) {
        return function(x) {
          return withArray(push(x))(xs)();
        };
      };
      singleton2 = function(a2) {
        return [a2];
      };
      replicate = /* @__PURE__ */ runFn2(replicateImpl);
      index = /* @__PURE__ */ function() {
        return runFn4(indexImpl)(Just.create)(Nothing.value);
      }();
      fromFoldable = function(dictFoldable) {
        return runFn2(fromFoldableImpl)(foldr(dictFoldable));
      };
      foldr2 = /* @__PURE__ */ foldr(foldableArray);
      findIndex = /* @__PURE__ */ function() {
        return runFn4(findIndexImpl)(Just.create)(Nothing.value);
      }();
      deleteAt = /* @__PURE__ */ function() {
        return runFn4(_deleteAt)(Just.create)(Nothing.value);
      }();
      deleteBy = function(v) {
        return function(v1) {
          return function(v2) {
            if (v2.length === 0) {
              return [];
            }
            ;
            return maybe(v2)(function(i2) {
              return fromJust2(deleteAt(i2)(v2));
            })(findIndex(v(v1))(v2));
          };
        };
      };
      concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));
      mapMaybe = function(f) {
        return concatMap(function() {
          var $189 = maybe([])(singleton2);
          return function($190) {
            return $189(f($190));
          };
        }());
      };
    }
  });

  // output/Effect.Aff/foreign.js
  function _catchError(aff) {
    return function(k) {
      return Aff.Catch(aff, k);
    };
  }
  function _map(f) {
    return function(aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function(value12) {
          return Aff.Pure(f(value12));
        });
      }
    };
  }
  function _bind(aff) {
    return function(k) {
      return Aff.Bind(aff, k);
    };
  }
  function _fork(immediate) {
    return function(aff) {
      return Aff.Fork(immediate, aff);
    };
  }
  function _parAffMap(f) {
    return function(aff) {
      return Aff.ParMap(f, aff);
    };
  }
  function _parAffApply(aff1) {
    return function(aff2) {
      return Aff.ParApply(aff1, aff2);
    };
  }
  function generalBracket(acquire) {
    return function(options2) {
      return function(k) {
        return Aff.Bracket(acquire, options2, k);
      };
    };
  }
  function _makeFiber(util, aff) {
    return function() {
      return Aff.Fiber(util, null, aff);
    };
  }
  var Aff, _pure, _throwError, _liftEffect, makeAff, _delay, _sequential;
  var init_foreign33 = __esm({
    "output/Effect.Aff/foreign.js"() {
      Aff = function() {
        var EMPTY = {};
        var PURE = "Pure";
        var THROW = "Throw";
        var CATCH = "Catch";
        var SYNC = "Sync";
        var ASYNC = "Async";
        var BIND = "Bind";
        var BRACKET = "Bracket";
        var FORK = "Fork";
        var SEQ = "Sequential";
        var MAP = "Map";
        var APPLY = "Apply";
        var ALT = "Alt";
        var CONS = "Cons";
        var RESUME = "Resume";
        var RELEASE = "Release";
        var FINALIZER = "Finalizer";
        var FINALIZED = "Finalized";
        var FORKED = "Forked";
        var FIBER = "Fiber";
        var THUNK = "Thunk";
        function Aff2(tag, _12, _2, _3) {
          this.tag = tag;
          this._1 = _12;
          this._2 = _2;
          this._3 = _3;
        }
        function AffCtr(tag) {
          var fn = function(_12, _2, _3) {
            return new Aff2(tag, _12, _2, _3);
          };
          fn.tag = tag;
          return fn;
        }
        function nonCanceler2(error4) {
          return new Aff2(PURE, void 0);
        }
        function runEff(eff) {
          try {
            eff();
          } catch (error4) {
            setTimeout(function() {
              throw error4;
            }, 0);
          }
        }
        function runSync(left, right, eff) {
          try {
            return right(eff());
          } catch (error4) {
            return left(error4);
          }
        }
        function runAsync(left, eff, k) {
          try {
            return eff(k)();
          } catch (error4) {
            k(left(error4))();
            return nonCanceler2;
          }
        }
        var Scheduler = function() {
          var limit = 1024;
          var size4 = 0;
          var ix = 0;
          var queue = new Array(limit);
          var draining = false;
          function drain() {
            var thunk;
            draining = true;
            while (size4 !== 0) {
              size4--;
              thunk = queue[ix];
              queue[ix] = void 0;
              ix = (ix + 1) % limit;
              thunk();
            }
            draining = false;
          }
          return {
            isDraining: function() {
              return draining;
            },
            enqueue: function(cb) {
              var i2, tmp;
              if (size4 === limit) {
                tmp = draining;
                drain();
                draining = tmp;
              }
              queue[(ix + size4) % limit] = cb;
              size4++;
              if (!draining) {
                drain();
              }
            }
          };
        }();
        function Supervisor(util) {
          var fibers = {};
          var fiberId = 0;
          var count = 0;
          return {
            register: function(fiber) {
              var fid = fiberId++;
              fiber.onComplete({
                rethrow: true,
                handler: function(result) {
                  return function() {
                    count--;
                    delete fibers[fid];
                  };
                }
              })();
              fibers[fid] = fiber;
              count++;
            },
            isEmpty: function() {
              return count === 0;
            },
            killAll: function(killError, cb) {
              return function() {
                if (count === 0) {
                  return cb();
                }
                var killCount = 0;
                var kills = {};
                function kill2(fid) {
                  kills[fid] = fibers[fid].kill(killError, function(result) {
                    return function() {
                      delete kills[fid];
                      killCount--;
                      if (util.isLeft(result) && util.fromLeft(result)) {
                        setTimeout(function() {
                          throw util.fromLeft(result);
                        }, 0);
                      }
                      if (killCount === 0) {
                        cb();
                      }
                    };
                  })();
                }
                for (var k in fibers) {
                  if (fibers.hasOwnProperty(k)) {
                    killCount++;
                    kill2(k);
                  }
                }
                fibers = {};
                fiberId = 0;
                count = 0;
                return function(error4) {
                  return new Aff2(SYNC, function() {
                    for (var k2 in kills) {
                      if (kills.hasOwnProperty(k2)) {
                        kills[k2]();
                      }
                    }
                  });
                };
              };
            }
          };
        }
        var SUSPENDED = 0;
        var CONTINUE = 1;
        var STEP_BIND = 2;
        var STEP_RESULT = 3;
        var PENDING = 4;
        var RETURN = 5;
        var COMPLETED = 6;
        function Fiber(util, supervisor, aff) {
          var runTick = 0;
          var status = SUSPENDED;
          var step4 = aff;
          var fail2 = null;
          var interrupt = null;
          var bhead = null;
          var btail = null;
          var attempts = null;
          var bracketCount = 0;
          var joinId = 0;
          var joins = null;
          var rethrow = true;
          function run3(localRunTick) {
            var tmp, result, attempt;
            while (true) {
              tmp = null;
              result = null;
              attempt = null;
              switch (status) {
                case STEP_BIND:
                  status = CONTINUE;
                  try {
                    step4 = bhead(step4);
                    if (btail === null) {
                      bhead = null;
                    } else {
                      bhead = btail._1;
                      btail = btail._2;
                    }
                  } catch (e) {
                    status = RETURN;
                    fail2 = util.left(e);
                    step4 = null;
                  }
                  break;
                case STEP_RESULT:
                  if (util.isLeft(step4)) {
                    status = RETURN;
                    fail2 = step4;
                    step4 = null;
                  } else if (bhead === null) {
                    status = RETURN;
                  } else {
                    status = STEP_BIND;
                    step4 = util.fromRight(step4);
                  }
                  break;
                case CONTINUE:
                  switch (step4.tag) {
                    case BIND:
                      if (bhead) {
                        btail = new Aff2(CONS, bhead, btail);
                      }
                      bhead = step4._2;
                      status = CONTINUE;
                      step4 = step4._1;
                      break;
                    case PURE:
                      if (bhead === null) {
                        status = RETURN;
                        step4 = util.right(step4._1);
                      } else {
                        status = STEP_BIND;
                        step4 = step4._1;
                      }
                      break;
                    case SYNC:
                      status = STEP_RESULT;
                      step4 = runSync(util.left, util.right, step4._1);
                      break;
                    case ASYNC:
                      status = PENDING;
                      step4 = runAsync(util.left, step4._1, function(result2) {
                        return function() {
                          if (runTick !== localRunTick) {
                            return;
                          }
                          runTick++;
                          Scheduler.enqueue(function() {
                            if (runTick !== localRunTick + 1) {
                              return;
                            }
                            status = STEP_RESULT;
                            step4 = result2;
                            run3(runTick);
                          });
                        };
                      });
                      return;
                    case THROW:
                      status = RETURN;
                      fail2 = util.left(step4._1);
                      step4 = null;
                      break;
                    case CATCH:
                      if (bhead === null) {
                        attempts = new Aff2(CONS, step4, attempts, interrupt);
                      } else {
                        attempts = new Aff2(CONS, step4, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                      }
                      bhead = null;
                      btail = null;
                      status = CONTINUE;
                      step4 = step4._1;
                      break;
                    case BRACKET:
                      bracketCount++;
                      if (bhead === null) {
                        attempts = new Aff2(CONS, step4, attempts, interrupt);
                      } else {
                        attempts = new Aff2(CONS, step4, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                      }
                      bhead = null;
                      btail = null;
                      status = CONTINUE;
                      step4 = step4._1;
                      break;
                    case FORK:
                      status = STEP_RESULT;
                      tmp = Fiber(util, supervisor, step4._2);
                      if (supervisor) {
                        supervisor.register(tmp);
                      }
                      if (step4._1) {
                        tmp.run();
                      }
                      step4 = util.right(tmp);
                      break;
                    case SEQ:
                      status = CONTINUE;
                      step4 = sequential3(util, supervisor, step4._1);
                      break;
                  }
                  break;
                case RETURN:
                  bhead = null;
                  btail = null;
                  if (attempts === null) {
                    status = COMPLETED;
                    step4 = interrupt || fail2 || step4;
                  } else {
                    tmp = attempts._3;
                    attempt = attempts._1;
                    attempts = attempts._2;
                    switch (attempt.tag) {
                      case CATCH:
                        if (interrupt && interrupt !== tmp && bracketCount === 0) {
                          status = RETURN;
                        } else if (fail2) {
                          status = CONTINUE;
                          step4 = attempt._2(util.fromLeft(fail2));
                          fail2 = null;
                        }
                        break;
                      case RESUME:
                        if (interrupt && interrupt !== tmp && bracketCount === 0 || fail2) {
                          status = RETURN;
                        } else {
                          bhead = attempt._1;
                          btail = attempt._2;
                          status = STEP_BIND;
                          step4 = util.fromRight(step4);
                        }
                        break;
                      case BRACKET:
                        bracketCount--;
                        if (fail2 === null) {
                          result = util.fromRight(step4);
                          attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                          if (interrupt === tmp || bracketCount > 0) {
                            status = CONTINUE;
                            step4 = attempt._3(result);
                          }
                        }
                        break;
                      case RELEASE:
                        attempts = new Aff2(CONS, new Aff2(FINALIZED, step4, fail2), attempts, interrupt);
                        status = CONTINUE;
                        if (interrupt && interrupt !== tmp && bracketCount === 0) {
                          step4 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                        } else if (fail2) {
                          step4 = attempt._1.failed(util.fromLeft(fail2))(attempt._2);
                        } else {
                          step4 = attempt._1.completed(util.fromRight(step4))(attempt._2);
                        }
                        fail2 = null;
                        bracketCount++;
                        break;
                      case FINALIZER:
                        bracketCount++;
                        attempts = new Aff2(CONS, new Aff2(FINALIZED, step4, fail2), attempts, interrupt);
                        status = CONTINUE;
                        step4 = attempt._1;
                        break;
                      case FINALIZED:
                        bracketCount--;
                        status = RETURN;
                        step4 = attempt._1;
                        fail2 = attempt._2;
                        break;
                    }
                  }
                  break;
                case COMPLETED:
                  for (var k in joins) {
                    if (joins.hasOwnProperty(k)) {
                      rethrow = rethrow && joins[k].rethrow;
                      runEff(joins[k].handler(step4));
                    }
                  }
                  joins = null;
                  if (interrupt && fail2) {
                    setTimeout(function() {
                      throw util.fromLeft(fail2);
                    }, 0);
                  } else if (util.isLeft(step4) && rethrow) {
                    setTimeout(function() {
                      if (rethrow) {
                        throw util.fromLeft(step4);
                      }
                    }, 0);
                  }
                  return;
                case SUSPENDED:
                  status = CONTINUE;
                  break;
                case PENDING:
                  return;
              }
            }
          }
          function onComplete(join4) {
            return function() {
              if (status === COMPLETED) {
                rethrow = rethrow && join4.rethrow;
                join4.handler(step4)();
                return function() {
                };
              }
              var jid = joinId++;
              joins = joins || {};
              joins[jid] = join4;
              return function() {
                if (joins !== null) {
                  delete joins[jid];
                }
              };
            };
          }
          function kill2(error4, cb) {
            return function() {
              if (status === COMPLETED) {
                cb(util.right(void 0))();
                return function() {
                };
              }
              var canceler = onComplete({
                rethrow: false,
                handler: function() {
                  return cb(util.right(void 0));
                }
              })();
              switch (status) {
                case SUSPENDED:
                  interrupt = util.left(error4);
                  status = COMPLETED;
                  step4 = interrupt;
                  run3(runTick);
                  break;
                case PENDING:
                  if (interrupt === null) {
                    interrupt = util.left(error4);
                  }
                  if (bracketCount === 0) {
                    if (status === PENDING) {
                      attempts = new Aff2(CONS, new Aff2(FINALIZER, step4(error4)), attempts, interrupt);
                    }
                    status = RETURN;
                    step4 = null;
                    fail2 = null;
                    run3(++runTick);
                  }
                  break;
                default:
                  if (interrupt === null) {
                    interrupt = util.left(error4);
                  }
                  if (bracketCount === 0) {
                    status = RETURN;
                    step4 = null;
                    fail2 = null;
                  }
              }
              return canceler;
            };
          }
          function join3(cb) {
            return function() {
              var canceler = onComplete({
                rethrow: false,
                handler: cb
              })();
              if (status === SUSPENDED) {
                run3(runTick);
              }
              return canceler;
            };
          }
          return {
            kill: kill2,
            join: join3,
            onComplete,
            isSuspended: function() {
              return status === SUSPENDED;
            },
            run: function() {
              if (status === SUSPENDED) {
                if (!Scheduler.isDraining()) {
                  Scheduler.enqueue(function() {
                    run3(runTick);
                  });
                } else {
                  run3(runTick);
                }
              }
            }
          };
        }
        function runPar(util, supervisor, par, cb) {
          var fiberId = 0;
          var fibers = {};
          var killId = 0;
          var kills = {};
          var early = new Error("[ParAff] Early exit");
          var interrupt = null;
          var root = EMPTY;
          function kill2(error4, par2, cb2) {
            var step4 = par2;
            var head3 = null;
            var tail = null;
            var count = 0;
            var kills2 = {};
            var tmp, kid;
            loop:
              while (true) {
                tmp = null;
                switch (step4.tag) {
                  case FORKED:
                    if (step4._3 === EMPTY) {
                      tmp = fibers[step4._1];
                      kills2[count++] = tmp.kill(error4, function(result) {
                        return function() {
                          count--;
                          if (count === 0) {
                            cb2(result)();
                          }
                        };
                      });
                    }
                    if (head3 === null) {
                      break loop;
                    }
                    step4 = head3._2;
                    if (tail === null) {
                      head3 = null;
                    } else {
                      head3 = tail._1;
                      tail = tail._2;
                    }
                    break;
                  case MAP:
                    step4 = step4._2;
                    break;
                  case APPLY:
                  case ALT:
                    if (head3) {
                      tail = new Aff2(CONS, head3, tail);
                    }
                    head3 = step4;
                    step4 = step4._1;
                    break;
                }
              }
            if (count === 0) {
              cb2(util.right(void 0))();
            } else {
              kid = 0;
              tmp = count;
              for (; kid < tmp; kid++) {
                kills2[kid] = kills2[kid]();
              }
            }
            return kills2;
          }
          function join3(result, head3, tail) {
            var fail2, step4, lhs, rhs, tmp, kid;
            if (util.isLeft(result)) {
              fail2 = result;
              step4 = null;
            } else {
              step4 = result;
              fail2 = null;
            }
            loop:
              while (true) {
                lhs = null;
                rhs = null;
                tmp = null;
                kid = null;
                if (interrupt !== null) {
                  return;
                }
                if (head3 === null) {
                  cb(fail2 || step4)();
                  return;
                }
                if (head3._3 !== EMPTY) {
                  return;
                }
                switch (head3.tag) {
                  case MAP:
                    if (fail2 === null) {
                      head3._3 = util.right(head3._1(util.fromRight(step4)));
                      step4 = head3._3;
                    } else {
                      head3._3 = fail2;
                    }
                    break;
                  case APPLY:
                    lhs = head3._1._3;
                    rhs = head3._2._3;
                    if (fail2) {
                      head3._3 = fail2;
                      tmp = true;
                      kid = killId++;
                      kills[kid] = kill2(early, fail2 === lhs ? head3._2 : head3._1, function() {
                        return function() {
                          delete kills[kid];
                          if (tmp) {
                            tmp = false;
                          } else if (tail === null) {
                            join3(fail2, null, null);
                          } else {
                            join3(fail2, tail._1, tail._2);
                          }
                        };
                      });
                      if (tmp) {
                        tmp = false;
                        return;
                      }
                    } else if (lhs === EMPTY || rhs === EMPTY) {
                      return;
                    } else {
                      step4 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                      head3._3 = step4;
                    }
                    break;
                  case ALT:
                    lhs = head3._1._3;
                    rhs = head3._2._3;
                    if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                      return;
                    }
                    if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                      fail2 = step4 === lhs ? rhs : lhs;
                      step4 = null;
                      head3._3 = fail2;
                    } else {
                      head3._3 = step4;
                      tmp = true;
                      kid = killId++;
                      kills[kid] = kill2(early, step4 === lhs ? head3._2 : head3._1, function() {
                        return function() {
                          delete kills[kid];
                          if (tmp) {
                            tmp = false;
                          } else if (tail === null) {
                            join3(step4, null, null);
                          } else {
                            join3(step4, tail._1, tail._2);
                          }
                        };
                      });
                      if (tmp) {
                        tmp = false;
                        return;
                      }
                    }
                    break;
                }
                if (tail === null) {
                  head3 = null;
                } else {
                  head3 = tail._1;
                  tail = tail._2;
                }
              }
          }
          function resolve(fiber) {
            return function(result) {
              return function() {
                delete fibers[fiber._1];
                fiber._3 = result;
                join3(result, fiber._2._1, fiber._2._2);
              };
            };
          }
          function run3() {
            var status = CONTINUE;
            var step4 = par;
            var head3 = null;
            var tail = null;
            var tmp, fid;
            loop:
              while (true) {
                tmp = null;
                fid = null;
                switch (status) {
                  case CONTINUE:
                    switch (step4.tag) {
                      case MAP:
                        if (head3) {
                          tail = new Aff2(CONS, head3, tail);
                        }
                        head3 = new Aff2(MAP, step4._1, EMPTY, EMPTY);
                        step4 = step4._2;
                        break;
                      case APPLY:
                        if (head3) {
                          tail = new Aff2(CONS, head3, tail);
                        }
                        head3 = new Aff2(APPLY, EMPTY, step4._2, EMPTY);
                        step4 = step4._1;
                        break;
                      case ALT:
                        if (head3) {
                          tail = new Aff2(CONS, head3, tail);
                        }
                        head3 = new Aff2(ALT, EMPTY, step4._2, EMPTY);
                        step4 = step4._1;
                        break;
                      default:
                        fid = fiberId++;
                        status = RETURN;
                        tmp = step4;
                        step4 = new Aff2(FORKED, fid, new Aff2(CONS, head3, tail), EMPTY);
                        tmp = Fiber(util, supervisor, tmp);
                        tmp.onComplete({
                          rethrow: false,
                          handler: resolve(step4)
                        })();
                        fibers[fid] = tmp;
                        if (supervisor) {
                          supervisor.register(tmp);
                        }
                    }
                    break;
                  case RETURN:
                    if (head3 === null) {
                      break loop;
                    }
                    if (head3._1 === EMPTY) {
                      head3._1 = step4;
                      status = CONTINUE;
                      step4 = head3._2;
                      head3._2 = EMPTY;
                    } else {
                      head3._2 = step4;
                      step4 = head3;
                      if (tail === null) {
                        head3 = null;
                      } else {
                        head3 = tail._1;
                        tail = tail._2;
                      }
                    }
                }
              }
            root = step4;
            for (fid = 0; fid < fiberId; fid++) {
              fibers[fid].run();
            }
          }
          function cancel(error4, cb2) {
            interrupt = util.left(error4);
            var innerKills;
            for (var kid in kills) {
              if (kills.hasOwnProperty(kid)) {
                innerKills = kills[kid];
                for (kid in innerKills) {
                  if (innerKills.hasOwnProperty(kid)) {
                    innerKills[kid]();
                  }
                }
              }
            }
            kills = null;
            var newKills = kill2(error4, root, cb2);
            return function(killError) {
              return new Aff2(ASYNC, function(killCb) {
                return function() {
                  for (var kid2 in newKills) {
                    if (newKills.hasOwnProperty(kid2)) {
                      newKills[kid2]();
                    }
                  }
                  return nonCanceler2;
                };
              });
            };
          }
          run3();
          return function(killError) {
            return new Aff2(ASYNC, function(killCb) {
              return function() {
                return cancel(killError, killCb);
              };
            });
          };
        }
        function sequential3(util, supervisor, par) {
          return new Aff2(ASYNC, function(cb) {
            return function() {
              return runPar(util, supervisor, par, cb);
            };
          });
        }
        Aff2.EMPTY = EMPTY;
        Aff2.Pure = AffCtr(PURE);
        Aff2.Throw = AffCtr(THROW);
        Aff2.Catch = AffCtr(CATCH);
        Aff2.Sync = AffCtr(SYNC);
        Aff2.Async = AffCtr(ASYNC);
        Aff2.Bind = AffCtr(BIND);
        Aff2.Bracket = AffCtr(BRACKET);
        Aff2.Fork = AffCtr(FORK);
        Aff2.Seq = AffCtr(SEQ);
        Aff2.ParMap = AffCtr(MAP);
        Aff2.ParApply = AffCtr(APPLY);
        Aff2.ParAlt = AffCtr(ALT);
        Aff2.Fiber = Fiber;
        Aff2.Supervisor = Supervisor;
        Aff2.Scheduler = Scheduler;
        Aff2.nonCanceler = nonCanceler2;
        return Aff2;
      }();
      _pure = Aff.Pure;
      _throwError = Aff.Throw;
      _liftEffect = Aff.Sync;
      makeAff = Aff.Async;
      _delay = function() {
        function setDelay(n, k) {
          if (n === 0 && typeof setImmediate !== "undefined") {
            return setImmediate(k);
          } else {
            return setTimeout(k, n);
          }
        }
        function clearDelay(n, t) {
          if (n === 0 && typeof clearImmediate !== "undefined") {
            return clearImmediate(t);
          } else {
            return clearTimeout(t);
          }
        }
        return function(right, ms) {
          return Aff.Async(function(cb) {
            return function() {
              var timer = setDelay(ms, cb(right()));
              return function() {
                return Aff.Sync(function() {
                  return right(clearDelay(ms, timer));
                });
              };
            };
          });
        };
      }();
      _sequential = Aff.Seq;
    }
  });

  // output/Control.Monad.ST.Global/index.js
  var init_Control_Monad_ST3 = __esm({
    "output/Control.Monad.ST.Global/index.js"() {
      init_Unsafe();
    }
  });

  // output/Control.Monad.ST.Class/index.js
  var init_Control_Monad_ST4 = __esm({
    "output/Control.Monad.ST.Class/index.js"() {
      init_Control2();
      init_Control_Monad_ST3();
      init_Control_Monad_ST();
      init_Effect();
    }
  });

  // output/Control.Monad.Cont.Trans/index.js
  var init_Control_Monad_Cont2 = __esm({
    "output/Control.Monad.Cont.Trans/index.js"() {
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control_Monad_Cont();
      init_Control_Monad_Reader();
      init_Control_Monad_State();
      init_Control_Monad_Trans();
      init_Data2();
      init_Data19();
      init_Data7();
      init_Effect4();
      init_Control_Monad_Cont();
      init_Control_Monad_Trans();
    }
  });

  // output/Control.Monad.Maybe.Trans/index.js
  var init_Control_Monad_Maybe = __esm({
    "output/Control.Monad.Maybe.Trans/index.js"() {
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control2();
      init_Control6();
      init_Control_Monad_Cont();
      init_Control_Monad_Error();
      init_Control_Monad_Reader();
      init_Control_Monad_Rec();
      init_Control_Monad_State();
      init_Control_Monad_Trans();
      init_Control_Monad_Writer();
      init_Data4();
      init_Data15();
      init_Data19();
      init_Data7();
      init_Data22();
      init_Effect4();
      init_Control_Monad_Trans();
    }
  });

  // output/Type.Equality/index.js
  var init_Type2 = __esm({
    "output/Type.Equality/index.js"() {
    }
  });

  // output/Data.Distributive/index.js
  var init_Data33 = __esm({
    "output/Data.Distributive/index.js"() {
      init_Control2();
      init_Data4();
      init_Data20();
      init_Data25();
      init_Data22();
      init_Data3();
      init_Type2();
    }
  });

  // output/Control.Monad.Reader.Trans/index.js
  var init_Control_Monad_Reader2 = __esm({
    "output/Control.Monad.Reader.Trans/index.js"() {
      init_Control7();
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control_Monad_Cont();
      init_Control_Monad_Error();
      init_Control_Monad_Reader();
      init_Control_Monad_Rec();
      init_Control_Monad_State();
      init_Control_Monad_Trans();
      init_Control_Monad_Writer();
      init_Control9();
      init_Data33();
      init_Data2();
      init_Data4();
      init_Data19();
      init_Data7();
      init_Effect4();
      init_Control_Monad_Reader();
      init_Control_Monad_Trans();
    }
  });

  // output/Control.Monad.Writer.Trans/index.js
  var init_Control_Monad_Writer2 = __esm({
    "output/Control.Monad.Writer.Trans/index.js"() {
      init_Control7();
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control_Monad_Cont();
      init_Control_Monad_Error();
      init_Control_Monad_Reader();
      init_Control_Monad_Rec();
      init_Control_Monad_State();
      init_Control_Monad_Trans();
      init_Control_Monad_Writer();
      init_Control9();
      init_Data4();
      init_Data19();
      init_Data7();
      init_Data22();
      init_Data3();
      init_Effect4();
      init_Control_Monad_Trans();
      init_Control_Monad_Writer();
    }
  });

  // output/Data.Functor.Contravariant/index.js
  var init_Data_Functor6 = __esm({
    "output/Data.Functor.Contravariant/index.js"() {
      init_Data4();
      init_Data6();
    }
  });

  // output/Data.Profunctor/index.js
  var init_Data34 = __esm({
    "output/Data.Profunctor/index.js"() {
      init_Control2();
      init_Data25();
    }
  });

  // output/Data.Functor.Costar/index.js
  var init_Data_Functor7 = __esm({
    "output/Data.Functor.Costar/index.js"() {
      init_Control11();
      init_Control10();
      init_Data33();
      init_Data4();
      init_Data_Functor6();
      init_Data_Functor();
      init_Data34();
      init_Data22();
    }
  });

  // output/Data.Profunctor.Star/index.js
  var init_Data_Profunctor = __esm({
    "output/Data.Profunctor.Star/index.js"() {
      init_Control7();
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control9();
      init_Data33();
      init_Data16();
      init_Data4();
      init_Data_Functor();
      init_Data22();
    }
  });

  // output/Control.Parallel.Class/index.js
  var sequential, parallel;
  var init_Control_Parallel = __esm({
    "output/Control.Parallel.Class/index.js"() {
      init_Control4();
      init_Control5();
      init_Control_Monad_Cont2();
      init_Control_Monad_Except();
      init_Control_Monad_Maybe();
      init_Control_Monad_Reader2();
      init_Control_Monad_Writer2();
      init_Data16();
      init_Data4();
      init_Data_Functor4();
      init_Data_Functor7();
      init_Data15();
      init_Data_Profunctor();
      init_Data3();
      init_Effect4();
      init_Effect3();
      sequential = function(dict) {
        return dict.sequential;
      };
      parallel = function(dict) {
        return dict.parallel;
      };
    }
  });

  // output/Control.Parallel/index.js
  var identity5, parTraverse_, parSequence_;
  var init_Control12 = __esm({
    "output/Control.Parallel/index.js"() {
      init_Control3();
      init_Control2();
      init_Control_Parallel();
      init_Data26();
      init_Data29();
      init_Control_Parallel();
      identity5 = /* @__PURE__ */ identity(categoryFn);
      parTraverse_ = function(dictParallel) {
        var sequential3 = sequential(dictParallel);
        var traverse_7 = traverse_(dictParallel.Applicative1());
        var parallel3 = parallel(dictParallel);
        return function(dictFoldable) {
          var traverse_14 = traverse_7(dictFoldable);
          return function(f) {
            var $48 = traverse_14(function($50) {
              return parallel3(f($50));
            });
            return function($49) {
              return sequential3($48($49));
            };
          };
        };
      };
      parSequence_ = function(dictParallel) {
        var parTraverse_1 = parTraverse_(dictParallel);
        return function(dictFoldable) {
          return parTraverse_1(dictFoldable)(identity5);
        };
      };
    }
  });

  // output/Data.Time.Duration/index.js
  var init_Data_Time = __esm({
    "output/Data.Time.Duration/index.js"() {
      init_Control2();
      init_Data8();
      init_Data25();
      init_Data12();
      init_Data11();
      init_Data14();
    }
  });

  // output/Effect.Unsafe/foreign.js
  var unsafePerformEffect;
  var init_foreign34 = __esm({
    "output/Effect.Unsafe/foreign.js"() {
      unsafePerformEffect = function(f) {
        return f();
      };
    }
  });

  // output/Effect.Unsafe/index.js
  var init_Effect5 = __esm({
    "output/Effect.Unsafe/index.js"() {
      init_foreign34();
      init_foreign34();
    }
  });

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial;
  var init_foreign35 = __esm({
    "output/Partial.Unsafe/foreign.js"() {
      _unsafePartial = function(f) {
        return f();
      };
    }
  });

  // output/Partial/foreign.js
  var _crashWith;
  var init_foreign36 = __esm({
    "output/Partial/foreign.js"() {
      _crashWith = function(msg) {
        throw new Error(msg);
      };
    }
  });

  // output/Partial/index.js
  var crashWith;
  var init_Partial = __esm({
    "output/Partial/index.js"() {
      init_foreign36();
      crashWith = function() {
        return _crashWith;
      };
    }
  });

  // output/Partial.Unsafe/index.js
  var crashWith2, unsafePartial, unsafeCrashWith;
  var init_Partial2 = __esm({
    "output/Partial.Unsafe/index.js"() {
      init_foreign35();
      init_Partial();
      crashWith2 = /* @__PURE__ */ crashWith();
      unsafePartial = _unsafePartial;
      unsafeCrashWith = function(msg) {
        return unsafePartial(function() {
          return crashWith2(msg);
        });
      };
    }
  });

  // output/Effect.Aff/index.js
  var $runtime_lazy2, pure2, $$void3, map6, Canceler, suspendAff, functorParAff, functorAff, map1, forkAff, ffiUtil, makeFiber, launchAff, bracket, applyParAff, monadAff, bindAff, applicativeAff, $lazy_applyAff, pure22, bind1, bindFlipped3, $$finally, monadEffectAff, liftEffect2, effectCanceler, joinFiber, functorFiber, killFiber, monadThrowAff, monadErrorAff, $$try2, runAff, runAff_, parallelAff, $lazy_applicativeParAff, applicativeParAff, monadRecAff, nonCanceler;
  var init_Effect6 = __esm({
    "output/Effect.Aff/index.js"() {
      init_foreign33();
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control6();
      init_Control_Monad_Error();
      init_Control_Monad_Rec();
      init_Control_Monad_ST4();
      init_Control12();
      init_Control_Parallel();
      init_Control9();
      init_Data16();
      init_Data26();
      init_Data2();
      init_Data4();
      init_Data19();
      init_Data7();
      init_Data_Time();
      init_Data3();
      init_Effect();
      init_Effect4();
      init_Effect2();
      init_Effect5();
      init_Partial2();
      init_Unsafe();
      init_foreign33();
      init_Control_Monad_Error();
      init_Control_Parallel();
      init_Data_Time();
      init_Effect2();
      $runtime_lazy2 = function(name15, moduleName, init2) {
        var state3 = 0;
        var val;
        return function(lineNumber) {
          if (state3 === 2)
            return val;
          if (state3 === 1)
            throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
          state3 = 1;
          val = init2();
          state3 = 2;
          return val;
        };
      };
      pure2 = /* @__PURE__ */ pure(applicativeEffect);
      $$void3 = /* @__PURE__ */ $$void(functorEffect);
      map6 = /* @__PURE__ */ map(functorEffect);
      Canceler = function(x) {
        return x;
      };
      suspendAff = /* @__PURE__ */ _fork(false);
      functorParAff = {
        map: _parAffMap
      };
      functorAff = {
        map: _map
      };
      map1 = /* @__PURE__ */ map(functorAff);
      forkAff = /* @__PURE__ */ _fork(true);
      ffiUtil = /* @__PURE__ */ function() {
        var unsafeFromRight = function(v) {
          if (v instanceof Right) {
            return v.value0;
          }
          ;
          if (v instanceof Left) {
            return unsafeCrashWith("unsafeFromRight: Left");
          }
          ;
          throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): " + [v.constructor.name]);
        };
        var unsafeFromLeft = function(v) {
          if (v instanceof Left) {
            return v.value0;
          }
          ;
          if (v instanceof Right) {
            return unsafeCrashWith("unsafeFromLeft: Right");
          }
          ;
          throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): " + [v.constructor.name]);
        };
        var isLeft = function(v) {
          if (v instanceof Left) {
            return true;
          }
          ;
          if (v instanceof Right) {
            return false;
          }
          ;
          throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): " + [v.constructor.name]);
        };
        return {
          isLeft,
          fromLeft: unsafeFromLeft,
          fromRight: unsafeFromRight,
          left: Left.create,
          right: Right.create
        };
      }();
      makeFiber = function(aff) {
        return _makeFiber(ffiUtil, aff);
      };
      launchAff = function(aff) {
        return function __do2() {
          var fiber = makeFiber(aff)();
          fiber.run();
          return fiber;
        };
      };
      bracket = function(acquire) {
        return function(completed) {
          return generalBracket(acquire)({
            killed: $$const(completed),
            failed: $$const(completed),
            completed: $$const(completed)
          });
        };
      };
      applyParAff = {
        apply: _parAffApply,
        Functor0: function() {
          return functorParAff;
        }
      };
      monadAff = {
        Applicative0: function() {
          return applicativeAff;
        },
        Bind1: function() {
          return bindAff;
        }
      };
      bindAff = {
        bind: _bind,
        Apply0: function() {
          return $lazy_applyAff(0);
        }
      };
      applicativeAff = {
        pure: _pure,
        Apply0: function() {
          return $lazy_applyAff(0);
        }
      };
      $lazy_applyAff = /* @__PURE__ */ $runtime_lazy2("applyAff", "Effect.Aff", function() {
        return {
          apply: ap(monadAff),
          Functor0: function() {
            return functorAff;
          }
        };
      });
      pure22 = /* @__PURE__ */ pure(applicativeAff);
      bind1 = /* @__PURE__ */ bind(bindAff);
      bindFlipped3 = /* @__PURE__ */ bindFlipped(bindAff);
      $$finally = function(fin) {
        return function(a2) {
          return bracket(pure22(unit))($$const(fin))($$const(a2));
        };
      };
      monadEffectAff = {
        liftEffect: _liftEffect,
        Monad0: function() {
          return monadAff;
        }
      };
      liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
      effectCanceler = function($75) {
        return Canceler($$const(liftEffect2($75)));
      };
      joinFiber = function(v) {
        return makeAff(function(k) {
          return map6(effectCanceler)(v.join(k));
        });
      };
      functorFiber = {
        map: function(f) {
          return function(t) {
            return unsafePerformEffect(makeFiber(map1(f)(joinFiber(t))));
          };
        }
      };
      killFiber = function(e) {
        return function(v) {
          return bind1(liftEffect2(v.isSuspended))(function(suspended) {
            if (suspended) {
              return liftEffect2($$void3(v.kill(e, $$const(pure2(unit)))));
            }
            ;
            return makeAff(function(k) {
              return map6(effectCanceler)(v.kill(e, k));
            });
          });
        };
      };
      monadThrowAff = {
        throwError: _throwError,
        Monad0: function() {
          return monadAff;
        }
      };
      monadErrorAff = {
        catchError: _catchError,
        MonadThrow0: function() {
          return monadThrowAff;
        }
      };
      $$try2 = /* @__PURE__ */ $$try(monadErrorAff);
      runAff = function(k) {
        return function(aff) {
          return launchAff(bindFlipped3(function($80) {
            return liftEffect2(k($80));
          })($$try2(aff)));
        };
      };
      runAff_ = function(k) {
        return function(aff) {
          return $$void3(runAff(k)(aff));
        };
      };
      parallelAff = {
        parallel: unsafeCoerce2,
        sequential: _sequential,
        Monad0: function() {
          return monadAff;
        },
        Applicative1: function() {
          return $lazy_applicativeParAff(0);
        }
      };
      $lazy_applicativeParAff = /* @__PURE__ */ $runtime_lazy2("applicativeParAff", "Effect.Aff", function() {
        return {
          pure: function() {
            var $82 = parallel(parallelAff);
            return function($83) {
              return $82(pure22($83));
            };
          }(),
          Apply0: function() {
            return applyParAff;
          }
        };
      });
      applicativeParAff = /* @__PURE__ */ $lazy_applicativeParAff(136);
      monadRecAff = {
        tailRecM: function(k) {
          var go2 = function(a2) {
            return bind1(k(a2))(function(res) {
              if (res instanceof Done) {
                return pure22(res.value0);
              }
              ;
              if (res instanceof Loop) {
                return go2(res.value0);
              }
              ;
              throw new Error("Failed pattern match at Effect.Aff (line 104, column 7 - line 106, column 23): " + [res.constructor.name]);
            });
          };
          return go2;
        },
        Monad0: function() {
          return monadAff;
        }
      };
      nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure22(unit));
    }
  });

  // output/Data.Lazy/foreign.js
  var init_foreign37 = __esm({
    "output/Data.Lazy/foreign.js"() {
    }
  });

  // output/Data.Lazy/index.js
  var init_Data35 = __esm({
    "output/Data.Lazy/index.js"() {
      init_foreign37();
      init_Control3();
      init_Data13();
      init_Data8();
      init_Data18();
      init_Data26();
      init_Data2();
      init_Data4();
      init_Data_Functor();
      init_Data21();
      init_Data19();
      init_Data12();
      init_Data11();
      init_Data7();
      init_Data10();
      init_Data14();
      init_Data29();
      init_Data3();
      init_foreign37();
    }
  });

  // output/Control.Monad.List.Trans/index.js
  var init_Control_Monad_List = __esm({
    "output/Control.Monad.List.Trans/index.js"() {
      init_Control4();
      init_Control5();
      init_Control2();
      init_Control6();
      init_Control_Monad_Rec();
      init_Control_Monad_Trans();
      init_Data2();
      init_Data4();
      init_Data35();
      init_Data15();
      init_Data7();
      init_Data22();
      init_Data3();
      init_Effect4();
      init_Control_Monad_Trans();
    }
  });

  // output/Control.Monad.RWS.Trans/index.js
  var init_Control_Monad_RWS = __esm({
    "output/Control.Monad.RWS.Trans/index.js"() {
      init_Control7();
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control_Monad_Error();
      init_Control_Monad_Rec();
      init_Control_Monad_Trans();
      init_Control9();
      init_Data4();
      init_Data19();
      init_Data7();
      init_Data22();
      init_Data3();
      init_Effect4();
      init_Control_Monad_Trans();
    }
  });

  // output/Effect.Aff.Class/index.js
  var monadAffAff, liftAff;
  var init_Effect_Aff = __esm({
    "output/Effect.Aff.Class/index.js"() {
      init_Control2();
      init_Control_Monad_Cont2();
      init_Control_Monad_Except();
      init_Control_Monad_List();
      init_Control_Monad_Maybe();
      init_Control_Monad_RWS();
      init_Control_Monad_Reader2();
      init_Control_Monad_State2();
      init_Control_Monad_Trans();
      init_Control_Monad_Writer2();
      init_Effect6();
      monadAffAff = {
        liftAff: /* @__PURE__ */ identity(categoryFn),
        MonadEffect0: function() {
          return monadEffectAff;
        }
      };
      liftAff = function(dict) {
        return dict.liftAff;
      };
    }
  });

  // output/Effect.Uncurried/foreign.js
  var init_foreign38 = __esm({
    "output/Effect.Uncurried/foreign.js"() {
    }
  });

  // output/Effect.Uncurried/index.js
  var init_Effect7 = __esm({
    "output/Effect.Uncurried/index.js"() {
      init_foreign38();
      init_Data19();
      init_Data7();
      init_Effect();
      init_foreign38();
    }
  });

  // output/Effect.Aff.Compat/index.js
  var fromEffectFnAff;
  var init_Effect_Aff2 = __esm({
    "output/Effect.Aff.Compat/index.js"() {
      init_Data16();
      init_Effect6();
      init_Effect7();
      init_Effect7();
      fromEffectFnAff = function(v) {
        return makeAff(function(k) {
          return function __do2() {
            var v1 = v(function($9) {
              return k(Left.create($9))();
            }, function($10) {
              return k(Right.create($10))();
            });
            return function(e) {
              return makeAff(function(k2) {
                return function __do3() {
                  v1(e, function($11) {
                    return k2(Left.create($11))();
                  }, function($12) {
                    return k2(Right.create($12))();
                  });
                  return nonCanceler;
                };
              });
            };
          };
        });
      };
    }
  });

  // output/Web.DOM.ParentNode/foreign.js
  function _querySelector(selector) {
    return function(node) {
      return function() {
        return node.querySelector(selector);
      };
    };
  }
  var getEffProp, children, _firstElementChild, _lastElementChild, childElementCount;
  var init_foreign39 = __esm({
    "output/Web.DOM.ParentNode/foreign.js"() {
      getEffProp = function(name15) {
        return function(node) {
          return function() {
            return node[name15];
          };
        };
      };
      children = getEffProp("children");
      _firstElementChild = getEffProp("firstElementChild");
      _lastElementChild = getEffProp("lastElementChild");
      childElementCount = getEffProp("childElementCount");
    }
  });

  // output/Data.Nullable/foreign.js
  function nullable(a2, r, f) {
    return a2 == null ? r : f(a2);
  }
  function notNull(x) {
    return x;
  }
  var nullImpl;
  var init_foreign40 = __esm({
    "output/Data.Nullable/foreign.js"() {
      nullImpl = null;
    }
  });

  // output/Data.Nullable/index.js
  var toNullable, toMaybe;
  var init_Data36 = __esm({
    "output/Data.Nullable/index.js"() {
      init_foreign40();
      init_Data8();
      init_Data2();
      init_Data15();
      init_Data12();
      init_Data14();
      init_foreign40();
      toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
      toMaybe = function(n) {
        return nullable(n, Nothing.value, Just.create);
      };
    }
  });

  // output/Web.DOM.ParentNode/index.js
  var map7, querySelector;
  var init_Web_DOM = __esm({
    "output/Web.DOM.ParentNode/index.js"() {
      init_foreign39();
      init_Data8();
      init_Data4();
      init_Data36();
      init_Data12();
      init_Effect();
      init_foreign39();
      map7 = /* @__PURE__ */ map(functorEffect);
      querySelector = function(qs) {
        var $2 = map7(toMaybe);
        var $3 = _querySelector(qs);
        return function($4) {
          return $2($3($4));
        };
      };
    }
  });

  // output/Web.Event.EventTarget/foreign.js
  function eventListener(fn) {
    return function() {
      return function(event) {
        return fn(event)();
      };
    };
  }
  function addEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }
  function removeEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.removeEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }
  var init_foreign41 = __esm({
    "output/Web.Event.EventTarget/foreign.js"() {
    }
  });

  // output/Web.Event.EventTarget/index.js
  var init_Web_Event = __esm({
    "output/Web.Event.EventTarget/index.js"() {
      init_foreign41();
      init_foreign41();
    }
  });

  // output/Web.HTML/foreign.js
  var windowImpl;
  var init_foreign42 = __esm({
    "output/Web.HTML/foreign.js"() {
      windowImpl = function() {
        return window;
      };
    }
  });

  // output/Web.HTML.Common/index.js
  var init_Web_HTML = __esm({
    "output/Web.HTML.Common/index.js"() {
      init_Data8();
      init_Data12();
    }
  });

  // output/Web.HTML.HTMLAnchorElement/foreign.js
  var init_foreign43 = __esm({
    "output/Web.HTML.HTMLAnchorElement/foreign.js"() {
    }
  });

  // output/Web.Internal.FFI/foreign.js
  var init_foreign44 = __esm({
    "output/Web.Internal.FFI/foreign.js"() {
    }
  });

  // output/Web.Internal.FFI/index.js
  var init_Web_Internal = __esm({
    "output/Web.Internal.FFI/index.js"() {
      init_foreign44();
      init_Data15();
    }
  });

  // output/Web.HTML.HTMLAnchorElement/index.js
  var init_Web_HTML2 = __esm({
    "output/Web.HTML.HTMLAnchorElement/index.js"() {
      init_foreign43();
      init_Unsafe();
      init_Web_Internal();
      init_foreign43();
    }
  });

  // output/Web.HTML.HTMLAreaElement/foreign.js
  var init_foreign45 = __esm({
    "output/Web.HTML.HTMLAreaElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLAreaElement/index.js
  var init_Web_HTML3 = __esm({
    "output/Web.HTML.HTMLAreaElement/index.js"() {
      init_foreign45();
      init_Unsafe();
      init_Web_Internal();
      init_foreign45();
    }
  });

  // output/Web.HTML.HTMLAudioElement/foreign.js
  var init_foreign46 = __esm({
    "output/Web.HTML.HTMLAudioElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLAudioElement/index.js
  var init_Web_HTML4 = __esm({
    "output/Web.HTML.HTMLAudioElement/index.js"() {
      init_foreign46();
      init_Unsafe();
      init_Web_Internal();
      init_foreign46();
    }
  });

  // output/Web.HTML.HTMLBRElement/index.js
  var init_Web_HTML5 = __esm({
    "output/Web.HTML.HTMLBRElement/index.js"() {
      init_Unsafe();
      init_Web_Internal();
    }
  });

  // output/Web.HTML.HTMLBaseElement/foreign.js
  var init_foreign47 = __esm({
    "output/Web.HTML.HTMLBaseElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLBaseElement/index.js
  var init_Web_HTML6 = __esm({
    "output/Web.HTML.HTMLBaseElement/index.js"() {
      init_foreign47();
      init_Unsafe();
      init_Web_Internal();
      init_foreign47();
    }
  });

  // output/Web.HTML.HTMLBodyElement/index.js
  var init_Web_HTML7 = __esm({
    "output/Web.HTML.HTMLBodyElement/index.js"() {
      init_Unsafe();
      init_Web_Internal();
    }
  });

  // output/Web.HTML.HTMLButtonElement/foreign.js
  var init_foreign48 = __esm({
    "output/Web.HTML.HTMLButtonElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLButtonElement/index.js
  var init_Web_HTML8 = __esm({
    "output/Web.HTML.HTMLButtonElement/index.js"() {
      init_foreign48();
      init_Data4();
      init_Data36();
      init_Effect();
      init_Unsafe();
      init_Web_Internal();
      init_foreign48();
    }
  });

  // output/Web.HTML.HTMLCanvasElement/foreign.js
  var init_foreign49 = __esm({
    "output/Web.HTML.HTMLCanvasElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLCanvasElement/index.js
  var init_Web_HTML9 = __esm({
    "output/Web.HTML.HTMLCanvasElement/index.js"() {
      init_foreign49();
      init_Unsafe();
      init_Web_Internal();
      init_foreign49();
    }
  });

  // output/Web.HTML.HTMLDListElement/index.js
  var init_Web_HTML10 = __esm({
    "output/Web.HTML.HTMLDListElement/index.js"() {
      init_Unsafe();
      init_Web_Internal();
    }
  });

  // output/Web.HTML.HTMLDataElement/foreign.js
  var init_foreign50 = __esm({
    "output/Web.HTML.HTMLDataElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLDataElement/index.js
  var init_Web_HTML11 = __esm({
    "output/Web.HTML.HTMLDataElement/index.js"() {
      init_foreign50();
      init_Unsafe();
      init_Web_Internal();
      init_foreign50();
    }
  });

  // output/Web.HTML.HTMLDataListElement/foreign.js
  var init_foreign51 = __esm({
    "output/Web.HTML.HTMLDataListElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLDataListElement/index.js
  var init_Web_HTML12 = __esm({
    "output/Web.HTML.HTMLDataListElement/index.js"() {
      init_foreign51();
      init_Unsafe();
      init_Web_Internal();
      init_foreign51();
    }
  });

  // output/Web.HTML.HTMLDivElement/index.js
  var init_Web_HTML13 = __esm({
    "output/Web.HTML.HTMLDivElement/index.js"() {
      init_Unsafe();
      init_Web_Internal();
    }
  });

  // output/Web.HTML.HTMLDocument/foreign.js
  function _readyState(doc) {
    return doc.readyState;
  }
  var init_foreign52 = __esm({
    "output/Web.HTML.HTMLDocument/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLDocument.ReadyState/index.js
  var Loading, Interactive, Complete, parse;
  var init_Web_HTML_HTMLDocument = __esm({
    "output/Web.HTML.HTMLDocument.ReadyState/index.js"() {
      init_Data15();
      init_Data9();
      Loading = /* @__PURE__ */ function() {
        function Loading2() {
        }
        ;
        Loading2.value = new Loading2();
        return Loading2;
      }();
      Interactive = /* @__PURE__ */ function() {
        function Interactive2() {
        }
        ;
        Interactive2.value = new Interactive2();
        return Interactive2;
      }();
      Complete = /* @__PURE__ */ function() {
        function Complete2() {
        }
        ;
        Complete2.value = new Complete2();
        return Complete2;
      }();
      parse = function(v) {
        if (v === "loading") {
          return new Just(Loading.value);
        }
        ;
        if (v === "interactive") {
          return new Just(Interactive.value);
        }
        ;
        if (v === "complete") {
          return new Just(Complete.value);
        }
        ;
        return Nothing.value;
      };
    }
  });

  // output/Web.HTML.HTMLDocument.VisibilityState/index.js
  var init_Web_HTML_HTMLDocument2 = __esm({
    "output/Web.HTML.HTMLDocument.VisibilityState/index.js"() {
      init_Data15();
      init_Data9();
    }
  });

  // output/Web.HTML.HTMLDocument/index.js
  var map8, toParentNode, toDocument, readyState;
  var init_Web_HTML14 = __esm({
    "output/Web.HTML.HTMLDocument/index.js"() {
      init_foreign52();
      init_Data4();
      init_Data15();
      init_Data36();
      init_Effect();
      init_Unsafe();
      init_Web_HTML_HTMLDocument();
      init_Web_HTML_HTMLDocument2();
      init_Web_Internal();
      map8 = /* @__PURE__ */ map(functorEffect);
      toParentNode = unsafeCoerce2;
      toDocument = unsafeCoerce2;
      readyState = function(doc) {
        return map8(function() {
          var $4 = fromMaybe(Loading.value);
          return function($5) {
            return $4(parse($5));
          };
        }())(function() {
          return _readyState(doc);
        });
      };
    }
  });

  // output/Web.HTML.HTMLElement/foreign.js
  function _read(nothing, just, value12) {
    var tag = Object.prototype.toString.call(value12);
    if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
      return just(value12);
    } else {
      return nothing;
    }
  }
  var init_foreign53 = __esm({
    "output/Web.HTML.HTMLElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLElement/index.js
  var toNode, fromElement;
  var init_Web_HTML15 = __esm({
    "output/Web.HTML.HTMLElement/index.js"() {
      init_foreign53();
      init_Data4();
      init_Data15();
      init_Data36();
      init_Effect();
      init_Unsafe();
      init_foreign53();
      toNode = unsafeCoerce2;
      fromElement = function(x) {
        return _read(Nothing.value, Just.create, x);
      };
    }
  });

  // output/Web.HTML.HTMLEmbedElement/foreign.js
  var init_foreign54 = __esm({
    "output/Web.HTML.HTMLEmbedElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLEmbedElement/index.js
  var init_Web_HTML16 = __esm({
    "output/Web.HTML.HTMLEmbedElement/index.js"() {
      init_foreign54();
      init_Unsafe();
      init_Web_Internal();
      init_foreign54();
    }
  });

  // output/Web.HTML.HTMLFieldSetElement/foreign.js
  var init_foreign55 = __esm({
    "output/Web.HTML.HTMLFieldSetElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLFieldSetElement/index.js
  var init_Web_HTML17 = __esm({
    "output/Web.HTML.HTMLFieldSetElement/index.js"() {
      init_foreign55();
      init_Data4();
      init_Data36();
      init_Effect();
      init_Unsafe();
      init_Web_Internal();
      init_foreign55();
    }
  });

  // output/Web.HTML.HTMLFormElement/foreign.js
  var init_foreign56 = __esm({
    "output/Web.HTML.HTMLFormElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLFormElement/index.js
  var init_Web_HTML18 = __esm({
    "output/Web.HTML.HTMLFormElement/index.js"() {
      init_foreign56();
      init_Unsafe();
      init_Web_Internal();
      init_foreign56();
    }
  });

  // output/Web.HTML.HTMLHRElement/index.js
  var init_Web_HTML19 = __esm({
    "output/Web.HTML.HTMLHRElement/index.js"() {
      init_Unsafe();
      init_Web_Internal();
    }
  });

  // output/Web.HTML.HTMLHeadElement/index.js
  var init_Web_HTML20 = __esm({
    "output/Web.HTML.HTMLHeadElement/index.js"() {
      init_Unsafe();
      init_Web_Internal();
    }
  });

  // output/Web.HTML.HTMLHeadingElement/index.js
  var init_Web_HTML21 = __esm({
    "output/Web.HTML.HTMLHeadingElement/index.js"() {
      init_Unsafe();
      init_Web_Internal();
    }
  });

  // output/Web.HTML.HTMLIFrameElement/foreign.js
  var init_foreign57 = __esm({
    "output/Web.HTML.HTMLIFrameElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLIFrameElement/index.js
  var init_Web_HTML22 = __esm({
    "output/Web.HTML.HTMLIFrameElement/index.js"() {
      init_foreign57();
      init_Data4();
      init_Data36();
      init_Effect();
      init_Unsafe();
      init_Web_Internal();
      init_foreign57();
    }
  });

  // output/Web.HTML.HTMLImageElement/foreign.js
  var init_foreign58 = __esm({
    "output/Web.HTML.HTMLImageElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLImageElement.CORSMode/index.js
  var init_Web_HTML_HTMLImageElement = __esm({
    "output/Web.HTML.HTMLImageElement.CORSMode/index.js"() {
      init_Data15();
      init_Data9();
    }
  });

  // output/Web.HTML.HTMLImageElement.DecodingHint/index.js
  var init_Web_HTML_HTMLImageElement2 = __esm({
    "output/Web.HTML.HTMLImageElement.DecodingHint/index.js"() {
      init_Data15();
      init_Data9();
    }
  });

  // output/Web.HTML.HTMLImageElement.Laziness/index.js
  var init_Web_HTML_HTMLImageElement3 = __esm({
    "output/Web.HTML.HTMLImageElement.Laziness/index.js"() {
      init_Data15();
      init_Data9();
    }
  });

  // output/Web.HTML.HTMLImageElement/index.js
  var init_Web_HTML23 = __esm({
    "output/Web.HTML.HTMLImageElement/index.js"() {
      init_foreign58();
      init_Control5();
      init_Data4();
      init_Data15();
      init_Data36();
      init_Effect();
      init_Effect7();
      init_Unsafe();
      init_Web_HTML_HTMLImageElement();
      init_Web_HTML_HTMLImageElement2();
      init_Web_HTML_HTMLImageElement3();
      init_Web_Internal();
      init_foreign58();
    }
  });

  // output/Web.HTML.HTMLInputElement/foreign.js
  var init_foreign59 = __esm({
    "output/Web.HTML.HTMLInputElement/foreign.js"() {
    }
  });

  // output/Web.HTML.SelectionMode/index.js
  var init_Web_HTML24 = __esm({
    "output/Web.HTML.SelectionMode/index.js"() {
      init_Data15();
      init_Data9();
    }
  });

  // output/Web.HTML.HTMLInputElement/index.js
  var init_Web_HTML25 = __esm({
    "output/Web.HTML.HTMLInputElement/index.js"() {
      init_foreign59();
      init_Data4();
      init_Data36();
      init_Effect();
      init_Unsafe();
      init_Web_HTML24();
      init_Web_Internal();
      init_foreign59();
    }
  });

  // output/Web.HTML.HTMLKeygenElement/foreign.js
  var init_foreign60 = __esm({
    "output/Web.HTML.HTMLKeygenElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLKeygenElement/index.js
  var init_Web_HTML26 = __esm({
    "output/Web.HTML.HTMLKeygenElement/index.js"() {
      init_foreign60();
      init_Data4();
      init_Data36();
      init_Effect();
      init_Unsafe();
      init_Web_Internal();
      init_foreign60();
    }
  });

  // output/Web.HTML.HTMLLIElement/foreign.js
  var init_foreign61 = __esm({
    "output/Web.HTML.HTMLLIElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLLIElement/index.js
  var init_Web_HTML27 = __esm({
    "output/Web.HTML.HTMLLIElement/index.js"() {
      init_foreign61();
      init_Unsafe();
      init_Web_Internal();
      init_foreign61();
    }
  });

  // output/Web.HTML.HTMLLabelElement/foreign.js
  var init_foreign62 = __esm({
    "output/Web.HTML.HTMLLabelElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLLabelElement/index.js
  var init_Web_HTML28 = __esm({
    "output/Web.HTML.HTMLLabelElement/index.js"() {
      init_foreign62();
      init_Data4();
      init_Data36();
      init_Effect();
      init_Unsafe();
      init_Web_Internal();
      init_foreign62();
    }
  });

  // output/Web.HTML.HTMLLegendElement/foreign.js
  var init_foreign63 = __esm({
    "output/Web.HTML.HTMLLegendElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLLegendElement/index.js
  var init_Web_HTML29 = __esm({
    "output/Web.HTML.HTMLLegendElement/index.js"() {
      init_foreign63();
      init_Data4();
      init_Data36();
      init_Effect();
      init_Unsafe();
      init_Web_Internal();
    }
  });

  // output/Web.HTML.HTMLLinkElement/foreign.js
  var init_foreign64 = __esm({
    "output/Web.HTML.HTMLLinkElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLLinkElement/index.js
  var init_Web_HTML30 = __esm({
    "output/Web.HTML.HTMLLinkElement/index.js"() {
      init_foreign64();
      init_Unsafe();
      init_Web_Internal();
      init_foreign64();
    }
  });

  // output/Web.HTML.HTMLMapElement/foreign.js
  var init_foreign65 = __esm({
    "output/Web.HTML.HTMLMapElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLMapElement/index.js
  var init_Web_HTML31 = __esm({
    "output/Web.HTML.HTMLMapElement/index.js"() {
      init_foreign65();
      init_Unsafe();
      init_Web_Internal();
      init_foreign65();
    }
  });

  // output/Web.HTML.HTMLMediaElement/foreign.js
  var init_foreign66 = __esm({
    "output/Web.HTML.HTMLMediaElement/foreign.js"() {
    }
  });

  // output/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }
  var init_foreign67 = __esm({
    "output/Data.Enum/foreign.js"() {
    }
  });

  // output/Control.Alternative/index.js
  var init_Control13 = __esm({
    "output/Control.Alternative/index.js"() {
      init_Control7();
      init_Control4();
      init_Control3();
      init_Control9();
      init_Data4();
      init_Data3();
      init_Control7();
      init_Control4();
      init_Control3();
      init_Control9();
      init_Data4();
    }
  });

  // output/Data.Enum/index.js
  var bottom1, top1, toEnum, defaultSucc, defaultPred, charToEnum, enumChar, boundedEnumChar;
  var init_Data37 = __esm({
    "output/Data.Enum/index.js"() {
      init_foreign67();
      init_Control13();
      init_Control3();
      init_Control5();
      init_Data();
      init_Data13();
      init_Data16();
      init_Data8();
      init_Data2();
      init_Data4();
      init_Data15();
      init_Data12();
      init_Data9();
      init_Data14();
      init_Data22();
      init_Data31();
      init_Data30();
      init_Data3();
      bottom1 = /* @__PURE__ */ bottom(boundedChar);
      top1 = /* @__PURE__ */ top(boundedChar);
      toEnum = function(dict) {
        return dict.toEnum;
      };
      defaultSucc = function(toEnum$prime) {
        return function(fromEnum$prime) {
          return function(a2) {
            return toEnum$prime(fromEnum$prime(a2) + 1 | 0);
          };
        };
      };
      defaultPred = function(toEnum$prime) {
        return function(fromEnum$prime) {
          return function(a2) {
            return toEnum$prime(fromEnum$prime(a2) - 1 | 0);
          };
        };
      };
      charToEnum = function(v) {
        if (v >= toCharCode(bottom1) && v <= toCharCode(top1)) {
          return new Just(fromCharCode(v));
        }
        ;
        return Nothing.value;
      };
      enumChar = {
        succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
        pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
        Ord0: function() {
          return ordChar;
        }
      };
      boundedEnumChar = /* @__PURE__ */ function() {
        return {
          cardinality: toCharCode(top1) - toCharCode(bottom1) | 0,
          toEnum: charToEnum,
          fromEnum: toCharCode,
          Bounded0: function() {
            return boundedChar;
          },
          Enum1: function() {
            return enumChar;
          }
        };
      }();
    }
  });

  // output/Web.HTML.HTMLMediaElement.CanPlayType/index.js
  var init_Web_HTML_HTMLMediaElement = __esm({
    "output/Web.HTML.HTMLMediaElement.CanPlayType/index.js"() {
      init_Data15();
      init_Data9();
    }
  });

  // output/Web.HTML.HTMLMediaElement.NetworkState/index.js
  var init_Web_HTML_HTMLMediaElement2 = __esm({
    "output/Web.HTML.HTMLMediaElement.NetworkState/index.js"() {
      init_Data37();
      init_Data15();
      init_Data9();
    }
  });

  // output/Web.HTML.HTMLMediaElement.ReadyState/index.js
  var init_Web_HTML_HTMLMediaElement3 = __esm({
    "output/Web.HTML.HTMLMediaElement.ReadyState/index.js"() {
      init_Data37();
      init_Data15();
      init_Data9();
    }
  });

  // output/Web.HTML.HTMLMediaElement/index.js
  var init_Web_HTML32 = __esm({
    "output/Web.HTML.HTMLMediaElement/index.js"() {
      init_foreign66();
      init_Data37();
      init_Data4();
      init_Data15();
      init_Effect();
      init_Unsafe();
      init_Web_HTML_HTMLMediaElement();
      init_Web_HTML_HTMLMediaElement2();
      init_Web_HTML_HTMLMediaElement3();
      init_Web_Internal();
      init_foreign66();
    }
  });

  // output/Web.HTML.HTMLMetaElement/foreign.js
  var init_foreign68 = __esm({
    "output/Web.HTML.HTMLMetaElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLMetaElement/index.js
  var init_Web_HTML33 = __esm({
    "output/Web.HTML.HTMLMetaElement/index.js"() {
      init_foreign68();
      init_Unsafe();
      init_Web_Internal();
      init_foreign68();
    }
  });

  // output/Web.HTML.HTMLMeterElement/foreign.js
  var init_foreign69 = __esm({
    "output/Web.HTML.HTMLMeterElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLMeterElement/index.js
  var init_Web_HTML34 = __esm({
    "output/Web.HTML.HTMLMeterElement/index.js"() {
      init_foreign69();
      init_Unsafe();
      init_Web_Internal();
      init_foreign69();
    }
  });

  // output/Web.HTML.HTMLModElement/foreign.js
  var init_foreign70 = __esm({
    "output/Web.HTML.HTMLModElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLModElement/index.js
  var init_Web_HTML35 = __esm({
    "output/Web.HTML.HTMLModElement/index.js"() {
      init_foreign70();
      init_Unsafe();
      init_Web_Internal();
      init_foreign70();
    }
  });

  // output/Web.HTML.HTMLOListElement/foreign.js
  var init_foreign71 = __esm({
    "output/Web.HTML.HTMLOListElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLOListElement/index.js
  var init_Web_HTML36 = __esm({
    "output/Web.HTML.HTMLOListElement/index.js"() {
      init_foreign71();
      init_Unsafe();
      init_Web_Internal();
      init_foreign71();
    }
  });

  // output/Web.HTML.HTMLObjectElement/foreign.js
  var init_foreign72 = __esm({
    "output/Web.HTML.HTMLObjectElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLObjectElement/index.js
  var init_Web_HTML37 = __esm({
    "output/Web.HTML.HTMLObjectElement/index.js"() {
      init_foreign72();
      init_Data4();
      init_Data36();
      init_Effect();
      init_Unsafe();
      init_Web_Internal();
      init_foreign72();
    }
  });

  // output/Web.HTML.HTMLOptGroupElement/foreign.js
  var init_foreign73 = __esm({
    "output/Web.HTML.HTMLOptGroupElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLOptGroupElement/index.js
  var init_Web_HTML38 = __esm({
    "output/Web.HTML.HTMLOptGroupElement/index.js"() {
      init_foreign73();
      init_Unsafe();
      init_Web_Internal();
      init_foreign73();
    }
  });

  // output/Web.HTML.HTMLOptionElement/foreign.js
  var init_foreign74 = __esm({
    "output/Web.HTML.HTMLOptionElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLOptionElement/index.js
  var init_Web_HTML39 = __esm({
    "output/Web.HTML.HTMLOptionElement/index.js"() {
      init_foreign74();
      init_Data4();
      init_Data36();
      init_Effect();
      init_Unsafe();
      init_Web_Internal();
      init_foreign74();
    }
  });

  // output/Web.HTML.HTMLOutputElement/foreign.js
  var init_foreign75 = __esm({
    "output/Web.HTML.HTMLOutputElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLOutputElement/index.js
  var init_Web_HTML40 = __esm({
    "output/Web.HTML.HTMLOutputElement/index.js"() {
      init_foreign75();
      init_Data4();
      init_Data36();
      init_Effect();
      init_Unsafe();
      init_Web_Internal();
      init_foreign75();
    }
  });

  // output/Web.HTML.HTMLParagraphElement/index.js
  var init_Web_HTML41 = __esm({
    "output/Web.HTML.HTMLParagraphElement/index.js"() {
      init_Unsafe();
      init_Web_Internal();
    }
  });

  // output/Web.HTML.HTMLParamElement/foreign.js
  var init_foreign76 = __esm({
    "output/Web.HTML.HTMLParamElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLParamElement/index.js
  var init_Web_HTML42 = __esm({
    "output/Web.HTML.HTMLParamElement/index.js"() {
      init_foreign76();
      init_Unsafe();
      init_Web_Internal();
      init_foreign76();
    }
  });

  // output/Web.HTML.HTMLPreElement/index.js
  var init_Web_HTML43 = __esm({
    "output/Web.HTML.HTMLPreElement/index.js"() {
      init_Unsafe();
      init_Web_Internal();
    }
  });

  // output/Web.HTML.HTMLProgressElement/foreign.js
  var init_foreign77 = __esm({
    "output/Web.HTML.HTMLProgressElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLProgressElement/index.js
  var init_Web_HTML44 = __esm({
    "output/Web.HTML.HTMLProgressElement/index.js"() {
      init_foreign77();
      init_Unsafe();
      init_Web_Internal();
      init_foreign77();
    }
  });

  // output/Web.HTML.HTMLQuoteElement/foreign.js
  var init_foreign78 = __esm({
    "output/Web.HTML.HTMLQuoteElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLQuoteElement/index.js
  var init_Web_HTML45 = __esm({
    "output/Web.HTML.HTMLQuoteElement/index.js"() {
      init_foreign78();
      init_Unsafe();
      init_Web_Internal();
      init_foreign78();
    }
  });

  // output/Web.HTML.HTMLScriptElement/foreign.js
  var init_foreign79 = __esm({
    "output/Web.HTML.HTMLScriptElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLScriptElement/index.js
  var init_Web_HTML46 = __esm({
    "output/Web.HTML.HTMLScriptElement/index.js"() {
      init_foreign79();
      init_Unsafe();
      init_Web_Internal();
      init_foreign79();
    }
  });

  // output/Web.HTML.HTMLSelectElement/foreign.js
  var init_foreign80 = __esm({
    "output/Web.HTML.HTMLSelectElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLSelectElement/index.js
  var init_Web_HTML47 = __esm({
    "output/Web.HTML.HTMLSelectElement/index.js"() {
      init_foreign80();
      init_Data4();
      init_Data36();
      init_Effect();
      init_Unsafe();
      init_Web_Internal();
      init_foreign80();
    }
  });

  // output/Web.HTML.HTMLSourceElement/foreign.js
  var init_foreign81 = __esm({
    "output/Web.HTML.HTMLSourceElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLSourceElement/index.js
  var init_Web_HTML48 = __esm({
    "output/Web.HTML.HTMLSourceElement/index.js"() {
      init_foreign81();
      init_Unsafe();
      init_Web_Internal();
      init_foreign81();
    }
  });

  // output/Web.HTML.HTMLSpanElement/index.js
  var init_Web_HTML49 = __esm({
    "output/Web.HTML.HTMLSpanElement/index.js"() {
      init_Unsafe();
      init_Web_Internal();
    }
  });

  // output/Web.HTML.HTMLStyleElement/foreign.js
  var init_foreign82 = __esm({
    "output/Web.HTML.HTMLStyleElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLStyleElement/index.js
  var init_Web_HTML50 = __esm({
    "output/Web.HTML.HTMLStyleElement/index.js"() {
      init_foreign82();
      init_Unsafe();
      init_Web_Internal();
      init_foreign82();
    }
  });

  // output/Web.HTML.HTMLTableCaptionElement/index.js
  var init_Web_HTML51 = __esm({
    "output/Web.HTML.HTMLTableCaptionElement/index.js"() {
      init_Unsafe();
      init_Web_Internal();
    }
  });

  // output/Web.HTML.HTMLTableCellElement/foreign.js
  var init_foreign83 = __esm({
    "output/Web.HTML.HTMLTableCellElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLTableCellElement/index.js
  var init_Web_HTML52 = __esm({
    "output/Web.HTML.HTMLTableCellElement/index.js"() {
      init_foreign83();
      init_Unsafe();
      init_Web_Internal();
      init_foreign83();
    }
  });

  // output/Web.HTML.HTMLTableColElement/foreign.js
  var init_foreign84 = __esm({
    "output/Web.HTML.HTMLTableColElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLTableColElement/index.js
  var init_Web_HTML53 = __esm({
    "output/Web.HTML.HTMLTableColElement/index.js"() {
      init_foreign84();
      init_Unsafe();
      init_Web_Internal();
      init_foreign84();
    }
  });

  // output/Web.HTML.HTMLTableDataCellElement/index.js
  var init_Web_HTML54 = __esm({
    "output/Web.HTML.HTMLTableDataCellElement/index.js"() {
      init_Unsafe();
      init_Web_Internal();
    }
  });

  // output/Web.HTML.HTMLTableElement/foreign.js
  var init_foreign85 = __esm({
    "output/Web.HTML.HTMLTableElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLTableElement/index.js
  var init_Web_HTML55 = __esm({
    "output/Web.HTML.HTMLTableElement/index.js"() {
      init_foreign85();
      init_Data4();
      init_Data36();
      init_Effect();
      init_Unsafe();
      init_Web_Internal();
      init_foreign85();
    }
  });

  // output/Web.HTML.HTMLTableHeaderCellElement/foreign.js
  var init_foreign86 = __esm({
    "output/Web.HTML.HTMLTableHeaderCellElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLTableHeaderCellElement/index.js
  var init_Web_HTML56 = __esm({
    "output/Web.HTML.HTMLTableHeaderCellElement/index.js"() {
      init_foreign86();
      init_Unsafe();
      init_Web_Internal();
      init_foreign86();
    }
  });

  // output/Web.HTML.HTMLTableRowElement/foreign.js
  var init_foreign87 = __esm({
    "output/Web.HTML.HTMLTableRowElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLTableRowElement/index.js
  var init_Web_HTML57 = __esm({
    "output/Web.HTML.HTMLTableRowElement/index.js"() {
      init_foreign87();
      init_Unsafe();
      init_Web_Internal();
      init_foreign87();
    }
  });

  // output/Web.HTML.HTMLTableSectionElement/foreign.js
  var init_foreign88 = __esm({
    "output/Web.HTML.HTMLTableSectionElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLTableSectionElement/index.js
  var init_Web_HTML58 = __esm({
    "output/Web.HTML.HTMLTableSectionElement/index.js"() {
      init_foreign88();
      init_Unsafe();
      init_Web_Internal();
      init_foreign88();
    }
  });

  // output/Web.HTML.HTMLTemplateElement/foreign.js
  var init_foreign89 = __esm({
    "output/Web.HTML.HTMLTemplateElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLTemplateElement/index.js
  var init_Web_HTML59 = __esm({
    "output/Web.HTML.HTMLTemplateElement/index.js"() {
      init_foreign89();
      init_Unsafe();
      init_Web_Internal();
      init_foreign89();
    }
  });

  // output/Web.HTML.HTMLTextAreaElement/foreign.js
  var init_foreign90 = __esm({
    "output/Web.HTML.HTMLTextAreaElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLTextAreaElement/index.js
  var init_Web_HTML60 = __esm({
    "output/Web.HTML.HTMLTextAreaElement/index.js"() {
      init_foreign90();
      init_Data4();
      init_Data36();
      init_Effect();
      init_Unsafe();
      init_Web_HTML24();
      init_Web_Internal();
      init_foreign90();
    }
  });

  // output/Web.HTML.HTMLTimeElement/foreign.js
  var init_foreign91 = __esm({
    "output/Web.HTML.HTMLTimeElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLTimeElement/index.js
  var init_Web_HTML61 = __esm({
    "output/Web.HTML.HTMLTimeElement/index.js"() {
      init_foreign91();
      init_Unsafe();
      init_Web_Internal();
      init_foreign91();
    }
  });

  // output/Web.HTML.HTMLTitleElement/foreign.js
  var init_foreign92 = __esm({
    "output/Web.HTML.HTMLTitleElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLTitleElement/index.js
  var init_Web_HTML62 = __esm({
    "output/Web.HTML.HTMLTitleElement/index.js"() {
      init_foreign92();
      init_Unsafe();
      init_Web_Internal();
      init_foreign92();
    }
  });

  // output/Web.HTML.HTMLTrackElement/foreign.js
  var init_foreign93 = __esm({
    "output/Web.HTML.HTMLTrackElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLTrackElement.ReadyState/index.js
  var init_Web_HTML_HTMLTrackElement = __esm({
    "output/Web.HTML.HTMLTrackElement.ReadyState/index.js"() {
      init_Data37();
      init_Data15();
      init_Data9();
    }
  });

  // output/Web.HTML.HTMLTrackElement/index.js
  var init_Web_HTML63 = __esm({
    "output/Web.HTML.HTMLTrackElement/index.js"() {
      init_foreign93();
      init_Data37();
      init_Data4();
      init_Data15();
      init_Effect();
      init_Unsafe();
      init_Web_HTML_HTMLTrackElement();
      init_Web_Internal();
      init_foreign93();
    }
  });

  // output/Web.HTML.HTMLUListElement/index.js
  var init_Web_HTML64 = __esm({
    "output/Web.HTML.HTMLUListElement/index.js"() {
      init_Unsafe();
      init_Web_Internal();
    }
  });

  // output/Web.HTML.HTMLVideoElement/foreign.js
  var init_foreign94 = __esm({
    "output/Web.HTML.HTMLVideoElement/foreign.js"() {
    }
  });

  // output/Web.HTML.HTMLVideoElement/index.js
  var init_Web_HTML65 = __esm({
    "output/Web.HTML.HTMLVideoElement/index.js"() {
      init_foreign94();
      init_Unsafe();
      init_Web_Internal();
      init_foreign94();
    }
  });

  // output/Web.HTML.History/foreign.js
  var init_foreign95 = __esm({
    "output/Web.HTML.History/foreign.js"() {
    }
  });

  // output/Web.HTML.History/index.js
  var init_Web_HTML66 = __esm({
    "output/Web.HTML.History/index.js"() {
      init_foreign95();
      init_Data12();
      init_foreign95();
    }
  });

  // output/Web.HTML.Location/foreign.js
  var init_foreign96 = __esm({
    "output/Web.HTML.Location/foreign.js"() {
    }
  });

  // output/Web.HTML.Location/index.js
  var init_Web_HTML67 = __esm({
    "output/Web.HTML.Location/index.js"() {
      init_foreign96();
      init_foreign96();
    }
  });

  // output/Web.HTML.Navigator/foreign.js
  var init_foreign97 = __esm({
    "output/Web.HTML.Navigator/foreign.js"() {
    }
  });

  // output/Web.HTML.Navigator/index.js
  var init_Web_HTML68 = __esm({
    "output/Web.HTML.Navigator/index.js"() {
      init_foreign97();
      init_foreign97();
    }
  });

  // output/Web.HTML.Window/foreign.js
  function document(window2) {
    return function() {
      return window2.document;
    };
  }
  var init_foreign98 = __esm({
    "output/Web.HTML.Window/foreign.js"() {
    }
  });

  // output/Web.HTML.Window/index.js
  var toEventTarget;
  var init_Web_HTML69 = __esm({
    "output/Web.HTML.Window/index.js"() {
      init_foreign98();
      init_Data4();
      init_Data36();
      init_Data12();
      init_Effect();
      init_Unsafe();
      init_Web_Internal();
      init_foreign98();
      toEventTarget = unsafeCoerce2;
    }
  });

  // output/Web.HTML/index.js
  var init_Web = __esm({
    "output/Web.HTML/index.js"() {
      init_foreign42();
      init_Web_HTML();
      init_Web_HTML2();
      init_Web_HTML3();
      init_Web_HTML4();
      init_Web_HTML5();
      init_Web_HTML6();
      init_Web_HTML7();
      init_Web_HTML8();
      init_Web_HTML9();
      init_Web_HTML10();
      init_Web_HTML11();
      init_Web_HTML12();
      init_Web_HTML13();
      init_Web_HTML14();
      init_Web_HTML15();
      init_Web_HTML16();
      init_Web_HTML17();
      init_Web_HTML18();
      init_Web_HTML19();
      init_Web_HTML20();
      init_Web_HTML21();
      init_Web_HTML22();
      init_Web_HTML23();
      init_Web_HTML25();
      init_Web_HTML26();
      init_Web_HTML27();
      init_Web_HTML28();
      init_Web_HTML29();
      init_Web_HTML30();
      init_Web_HTML31();
      init_Web_HTML32();
      init_Web_HTML33();
      init_Web_HTML34();
      init_Web_HTML35();
      init_Web_HTML36();
      init_Web_HTML37();
      init_Web_HTML38();
      init_Web_HTML39();
      init_Web_HTML40();
      init_Web_HTML41();
      init_Web_HTML42();
      init_Web_HTML43();
      init_Web_HTML44();
      init_Web_HTML45();
      init_Web_HTML46();
      init_Web_HTML47();
      init_Web_HTML48();
      init_Web_HTML49();
      init_Web_HTML50();
      init_Web_HTML51();
      init_Web_HTML52();
      init_Web_HTML53();
      init_Web_HTML54();
      init_Web_HTML55();
      init_Web_HTML56();
      init_Web_HTML57();
      init_Web_HTML58();
      init_Web_HTML59();
      init_Web_HTML60();
      init_Web_HTML61();
      init_Web_HTML62();
      init_Web_HTML63();
      init_Web_HTML64();
      init_Web_HTML65();
      init_Web_HTML66();
      init_Web_HTML67();
      init_Web_HTML68();
      init_Web_HTML69();
      init_foreign42();
    }
  });

  // output/Web.HTML.Event.EventTypes/index.js
  var domcontentloaded, change;
  var init_Web_HTML_Event = __esm({
    "output/Web.HTML.Event.EventTypes/index.js"() {
      domcontentloaded = "DOMContentLoaded";
      change = "change";
    }
  });

  // output/Halogen.Aff.Util/index.js
  var bind2, liftEffect3, bindFlipped4, composeKleisliFlipped2, pure3, bindFlipped1, pure1, map9, discard2, throwError2, selectElement, runHalogenAff, awaitLoad, awaitBody;
  var init_Halogen_Aff = __esm({
    "output/Halogen.Aff.Util/index.js"() {
      init_Control4();
      init_Control5();
      init_Control_Monad_Error();
      init_Data16();
      init_Data2();
      init_Data4();
      init_Data15();
      init_Data3();
      init_Effect();
      init_Effect6();
      init_Effect4();
      init_Effect2();
      init_Web_DOM();
      init_Web_Event();
      init_Web();
      init_Web_HTML_Event();
      init_Web_HTML14();
      init_Web_HTML_HTMLDocument();
      init_Web_HTML15();
      init_Web_HTML69();
      bind2 = /* @__PURE__ */ bind(bindAff);
      liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectAff);
      bindFlipped4 = /* @__PURE__ */ bindFlipped(bindEffect);
      composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(bindEffect);
      pure3 = /* @__PURE__ */ pure(applicativeAff);
      bindFlipped1 = /* @__PURE__ */ bindFlipped(bindMaybe);
      pure1 = /* @__PURE__ */ pure(applicativeEffect);
      map9 = /* @__PURE__ */ map(functorEffect);
      discard2 = /* @__PURE__ */ discard(discardUnit);
      throwError2 = /* @__PURE__ */ throwError(monadThrowAff);
      selectElement = function(query2) {
        return bind2(liftEffect3(bindFlipped4(composeKleisliFlipped2(function() {
          var $16 = querySelector(query2);
          return function($17) {
            return $16(toParentNode($17));
          };
        }())(document))(windowImpl)))(function(mel) {
          return pure3(bindFlipped1(fromElement)(mel));
        });
      };
      runHalogenAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure1(unit))));
      awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
        return function __do2() {
          var rs = bindFlipped4(readyState)(bindFlipped4(document)(windowImpl))();
          if (rs instanceof Loading) {
            var et = map9(toEventTarget)(windowImpl)();
            var listener = eventListener(function(v) {
              return callback(new Right(unit));
            })();
            addEventListener(domcontentloaded)(listener)(false)(et)();
            return effectCanceler(removeEventListener(domcontentloaded)(listener)(false)(et));
          }
          ;
          callback(new Right(unit))();
          return nonCanceler;
        };
      });
      awaitBody = /* @__PURE__ */ discard2(bindAff)(awaitLoad)(function() {
        return bind2(selectElement("body"))(function(body2) {
          return maybe(throwError2(error("Could not find body")))(pure3)(body2);
        });
      });
    }
  });

  // output/Data.Exists/index.js
  var runExists, mkExists;
  var init_Data38 = __esm({
    "output/Data.Exists/index.js"() {
      init_Unsafe();
      runExists = unsafeCoerce2;
      mkExists = unsafeCoerce2;
    }
  });

  // output/Data.Coyoneda/index.js
  var CoyonedaF, unCoyoneda, coyoneda, functorCoyoneda, liftCoyoneda;
  var init_Data39 = __esm({
    "output/Data.Coyoneda/index.js"() {
      init_Control7();
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control2();
      init_Control11();
      init_Control10();
      init_Control9();
      init_Data33();
      init_Data8();
      init_Data38();
      init_Data26();
      init_Data4();
      init_Data_Functor();
      init_Data12();
      init_Data_Semigroup();
      init_Data_Semigroup2();
      init_Data29();
      CoyonedaF = /* @__PURE__ */ function() {
        function CoyonedaF2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        CoyonedaF2.create = function(value0) {
          return function(value1) {
            return new CoyonedaF2(value0, value1);
          };
        };
        return CoyonedaF2;
      }();
      unCoyoneda = function(f) {
        return function(v) {
          return runExists(function(v1) {
            return f(v1.value0)(v1.value1);
          })(v);
        };
      };
      coyoneda = function(k) {
        return function(fi) {
          return mkExists(new CoyonedaF(k, fi));
        };
      };
      functorCoyoneda = {
        map: function(f) {
          return function(v) {
            return runExists(function(v1) {
              return coyoneda(function($180) {
                return f(v1.value0($180));
              })(v1.value1);
            })(v);
          };
        }
      };
      liftCoyoneda = /* @__PURE__ */ coyoneda(/* @__PURE__ */ identity(categoryFn));
    }
  });

  // output/Data.FoldableWithIndex/index.js
  var init_Data40 = __esm({
    "output/Data.FoldableWithIndex/index.js"() {
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control2();
      init_Data16();
      init_Data26();
      init_Data2();
      init_Data_Functor2();
      init_Data28();
      init_Data15();
      init_Data19();
      init_Data_Monoid();
      init_Data_Monoid2();
      init_Data_Monoid3();
      init_Data_Monoid4();
      init_Data25();
      init_Data7();
      init_Data22();
      init_Data3();
    }
  });

  // output/Data.TraversableWithIndex/index.js
  var init_Data41 = __esm({
    "output/Data.TraversableWithIndex/index.js"() {
      init_Control4();
      init_Control3();
      init_Data16();
      init_Data40();
      init_Data2();
      init_Data4();
      init_Data_Functor3();
      init_Data_Functor4();
      init_Data_Functor2();
      init_Data_Functor5();
      init_Data28();
      init_Data20();
      init_Data29();
      init_Data_Traversable_Accum();
      init_Data22();
      init_Data3();
    }
  });

  // output/Data.NonEmpty/index.js
  var NonEmpty, singleton3;
  var init_Data42 = __esm({
    "output/Data.NonEmpty/index.js"() {
      init_Control7();
      init_Control4();
      init_Control3();
      init_Control9();
      init_Data8();
      init_Data26();
      init_Data40();
      init_Data4();
      init_Data28();
      init_Data15();
      init_Data12();
      init_Data9();
      init_Data7();
      init_Data_Semigroup();
      init_Data14();
      init_Data29();
      init_Data41();
      init_Data22();
      init_Data31();
      NonEmpty = /* @__PURE__ */ function() {
        function NonEmpty2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        NonEmpty2.create = function(value0) {
          return function(value1) {
            return new NonEmpty2(value0, value1);
          };
        };
        return NonEmpty2;
      }();
      singleton3 = function(dictPlus) {
        var empty8 = empty(dictPlus);
        return function(a2) {
          return new NonEmpty(a2, empty8);
        };
      };
    }
  });

  // output/Data.List.Types/index.js
  var Nil, Cons, NonEmptyList, listMap, functorList, foldableList, foldr3, semigroupList, append1, altList, plusList;
  var init_Data_List = __esm({
    "output/Data.List.Types/index.js"() {
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control2();
      init_Data8();
      init_Data26();
      init_Data40();
      init_Data2();
      init_Data4();
      init_Data28();
      init_Data15();
      init_Data19();
      init_Data42();
      init_Data12();
      init_Data9();
      init_Data7();
      init_Data_Semigroup2();
      init_Data10();
      init_Data14();
      init_Data29();
      init_Data41();
      init_Data22();
      Nil = /* @__PURE__ */ function() {
        function Nil3() {
        }
        ;
        Nil3.value = new Nil3();
        return Nil3;
      }();
      Cons = /* @__PURE__ */ function() {
        function Cons3(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        Cons3.create = function(value0) {
          return function(value1) {
            return new Cons3(value0, value1);
          };
        };
        return Cons3;
      }();
      NonEmptyList = function(x) {
        return x;
      };
      listMap = function(f) {
        var chunkedRevMap = function($copy_v) {
          return function($copy_v1) {
            var $tco_var_v = $copy_v;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v, v1) {
              if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Cons)) {
                $tco_var_v = new Cons(v1, v);
                $copy_v1 = v1.value1.value1.value1;
                return;
              }
              ;
              var unrolledMap = function(v2) {
                if (v2 instanceof Cons && (v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil)) {
                  return new Cons(f(v2.value0), new Cons(f(v2.value1.value0), Nil.value));
                }
                ;
                if (v2 instanceof Cons && v2.value1 instanceof Nil) {
                  return new Cons(f(v2.value0), Nil.value);
                }
                ;
                return Nil.value;
              };
              var reverseUnrolledMap = function($copy_v2) {
                return function($copy_v3) {
                  var $tco_var_v2 = $copy_v2;
                  var $tco_done1 = false;
                  var $tco_result2;
                  function $tco_loop2(v2, v3) {
                    if (v2 instanceof Cons && (v2.value0 instanceof Cons && (v2.value0.value1 instanceof Cons && v2.value0.value1.value1 instanceof Cons))) {
                      $tco_var_v2 = v2.value1;
                      $copy_v3 = new Cons(f(v2.value0.value0), new Cons(f(v2.value0.value1.value0), new Cons(f(v2.value0.value1.value1.value0), v3)));
                      return;
                    }
                    ;
                    $tco_done1 = true;
                    return v3;
                  }
                  ;
                  while (!$tco_done1) {
                    $tco_result2 = $tco_loop2($tco_var_v2, $copy_v3);
                  }
                  ;
                  return $tco_result2;
                };
              };
              $tco_done = true;
              return reverseUnrolledMap(v)(unrolledMap(v1));
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v, $copy_v1);
            }
            ;
            return $tco_result;
          };
        };
        return chunkedRevMap(Nil.value);
      };
      functorList = {
        map: listMap
      };
      foldableList = {
        foldr: function(f) {
          return function(b2) {
            var rev3 = function() {
              var go2 = function($copy_v) {
                return function($copy_v1) {
                  var $tco_var_v = $copy_v;
                  var $tco_done = false;
                  var $tco_result;
                  function $tco_loop(v, v1) {
                    if (v1 instanceof Nil) {
                      $tco_done = true;
                      return v;
                    }
                    ;
                    if (v1 instanceof Cons) {
                      $tco_var_v = new Cons(v1.value0, v);
                      $copy_v1 = v1.value1;
                      return;
                    }
                    ;
                    throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
                  }
                  ;
                  while (!$tco_done) {
                    $tco_result = $tco_loop($tco_var_v, $copy_v1);
                  }
                  ;
                  return $tco_result;
                };
              };
              return go2(Nil.value);
            }();
            var $284 = foldl(foldableList)(flip(f))(b2);
            return function($285) {
              return $284(rev3($285));
            };
          };
        },
        foldl: function(f) {
          var go2 = function($copy_b) {
            return function($copy_v) {
              var $tco_var_b = $copy_b;
              var $tco_done1 = false;
              var $tco_result;
              function $tco_loop(b2, v) {
                if (v instanceof Nil) {
                  $tco_done1 = true;
                  return b2;
                }
                ;
                if (v instanceof Cons) {
                  $tco_var_b = f(b2)(v.value0);
                  $copy_v = v.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
              }
              ;
              while (!$tco_done1) {
                $tco_result = $tco_loop($tco_var_b, $copy_v);
              }
              ;
              return $tco_result;
            };
          };
          return go2;
        },
        foldMap: function(dictMonoid) {
          var append22 = append(dictMonoid.Semigroup0());
          var mempty2 = mempty(dictMonoid);
          return function(f) {
            return foldl(foldableList)(function(acc) {
              var $286 = append22(acc);
              return function($287) {
                return $286(f($287));
              };
            })(mempty2);
          };
        }
      };
      foldr3 = /* @__PURE__ */ foldr(foldableList);
      semigroupList = {
        append: function(xs) {
          return function(ys) {
            return foldr3(Cons.create)(ys)(xs);
          };
        }
      };
      append1 = /* @__PURE__ */ append(semigroupList);
      altList = {
        alt: append1,
        Functor0: function() {
          return functorList;
        }
      };
      plusList = /* @__PURE__ */ function() {
        return {
          empty: Nil.value,
          Alt0: function() {
            return altList;
          }
        };
      }();
    }
  });

  // output/Data.List.Internal/index.js
  var init_Data_List2 = __esm({
    "output/Data.List.Internal/index.js"() {
      init_Data_List();
      init_Data9();
    }
  });

  // output/Data.List/index.js
  var bimap2, reverse2, $$null, manyRec;
  var init_Data43 = __esm({
    "output/Data.List/index.js"() {
      init_Control7();
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control2();
      init_Control8();
      init_Control_Monad_Rec();
      init_Data24();
      init_Data();
      init_Data8();
      init_Data26();
      init_Data2();
      init_Data4();
      init_Data21();
      init_Data_List2();
      init_Data_List();
      init_Data15();
      init_Data42();
      init_Data12();
      init_Data9();
      init_Data7();
      init_Data14();
      init_Data29();
      init_Data22();
      init_Data31();
      init_Data3();
      init_Data26();
      init_Data_List();
      init_Data29();
      bimap2 = /* @__PURE__ */ bimap(bifunctorStep);
      reverse2 = /* @__PURE__ */ function() {
        var go2 = function($copy_v) {
          return function($copy_v1) {
            var $tco_var_v = $copy_v;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v, v1) {
              if (v1 instanceof Nil) {
                $tco_done = true;
                return v;
              }
              ;
              if (v1 instanceof Cons) {
                $tco_var_v = new Cons(v1.value0, v);
                $copy_v1 = v1.value1;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [v.constructor.name, v1.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v, $copy_v1);
            }
            ;
            return $tco_result;
          };
        };
        return go2(Nil.value);
      }();
      $$null = function(v) {
        if (v instanceof Nil) {
          return true;
        }
        ;
        return false;
      };
      manyRec = function(dictMonadRec) {
        var bind15 = bind(dictMonadRec.Monad0().Bind1());
        var tailRecM4 = tailRecM(dictMonadRec);
        return function(dictAlternative) {
          var Alt0 = dictAlternative.Plus1().Alt0();
          var alt6 = alt(Alt0);
          var map110 = map(Alt0.Functor0());
          var pure11 = pure(dictAlternative.Applicative0());
          return function(p2) {
            var go2 = function(acc) {
              return bind15(alt6(map110(Loop.create)(p2))(pure11(new Done(unit))))(function(aa) {
                return pure11(bimap2(function(v) {
                  return new Cons(v, acc);
                })(function(v) {
                  return reverse2(acc);
                })(aa));
              });
            };
            return tailRecM4(go2)(Nil.value);
          };
        };
      };
    }
  });

  // output/Data.List.Lazy.Types/index.js
  var init_Data_List_Lazy = __esm({
    "output/Data.List.Lazy.Types/index.js"() {
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control2();
      init_Control8();
      init_Control6();
      init_Data8();
      init_Data26();
      init_Data40();
      init_Data2();
      init_Data4();
      init_Data28();
      init_Data35();
      init_Data15();
      init_Data19();
      init_Data25();
      init_Data42();
      init_Data12();
      init_Data9();
      init_Data7();
      init_Data10();
      init_Data14();
      init_Data29();
      init_Data41();
      init_Data22();
      init_Data30();
    }
  });

  // output/Data.List.Lazy/index.js
  var init_Data_List3 = __esm({
    "output/Data.List.Lazy/index.js"() {
      init_Control7();
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control2();
      init_Control8();
      init_Control_Monad_Rec();
      init_Data();
      init_Data8();
      init_Data26();
      init_Data2();
      init_Data4();
      init_Data21();
      init_Data35();
      init_Data_List2();
      init_Data_List_Lazy();
      init_Data15();
      init_Data25();
      init_Data42();
      init_Data12();
      init_Data9();
      init_Data7();
      init_Data14();
      init_Data29();
      init_Data22();
      init_Data31();
      init_Data26();
      init_Data_List_Lazy();
      init_Data29();
    }
  });

  // output/Data.Map.Internal/index.js
  var Leaf, Two, Three, TwoLeft, TwoRight, ThreeLeft, ThreeMiddle, ThreeRight, KickUp, lookup, fromZipper, insert, pop, foldableMap, empty2, $$delete, alter;
  var init_Data_Map = __esm({
    "output/Data.Map.Internal/index.js"() {
      init_Control4();
      init_Control3();
      init_Control2();
      init_Data8();
      init_Data26();
      init_Data40();
      init_Data2();
      init_Data4();
      init_Data28();
      init_Data21();
      init_Data43();
      init_Data_List3();
      init_Data_List_Lazy();
      init_Data_List();
      init_Data15();
      init_Data19();
      init_Data12();
      init_Data9();
      init_Data7();
      init_Data14();
      init_Data29();
      init_Data41();
      init_Data22();
      init_Data31();
      init_Partial2();
      Leaf = /* @__PURE__ */ function() {
        function Leaf2() {
        }
        ;
        Leaf2.value = new Leaf2();
        return Leaf2;
      }();
      Two = /* @__PURE__ */ function() {
        function Two2(value0, value1, value22, value32) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value22;
          this.value3 = value32;
        }
        ;
        Two2.create = function(value0) {
          return function(value1) {
            return function(value22) {
              return function(value32) {
                return new Two2(value0, value1, value22, value32);
              };
            };
          };
        };
        return Two2;
      }();
      Three = /* @__PURE__ */ function() {
        function Three2(value0, value1, value22, value32, value42, value52, value62) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value22;
          this.value3 = value32;
          this.value4 = value42;
          this.value5 = value52;
          this.value6 = value62;
        }
        ;
        Three2.create = function(value0) {
          return function(value1) {
            return function(value22) {
              return function(value32) {
                return function(value42) {
                  return function(value52) {
                    return function(value62) {
                      return new Three2(value0, value1, value22, value32, value42, value52, value62);
                    };
                  };
                };
              };
            };
          };
        };
        return Three2;
      }();
      TwoLeft = /* @__PURE__ */ function() {
        function TwoLeft2(value0, value1, value22) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value22;
        }
        ;
        TwoLeft2.create = function(value0) {
          return function(value1) {
            return function(value22) {
              return new TwoLeft2(value0, value1, value22);
            };
          };
        };
        return TwoLeft2;
      }();
      TwoRight = /* @__PURE__ */ function() {
        function TwoRight2(value0, value1, value22) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value22;
        }
        ;
        TwoRight2.create = function(value0) {
          return function(value1) {
            return function(value22) {
              return new TwoRight2(value0, value1, value22);
            };
          };
        };
        return TwoRight2;
      }();
      ThreeLeft = /* @__PURE__ */ function() {
        function ThreeLeft2(value0, value1, value22, value32, value42, value52) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value22;
          this.value3 = value32;
          this.value4 = value42;
          this.value5 = value52;
        }
        ;
        ThreeLeft2.create = function(value0) {
          return function(value1) {
            return function(value22) {
              return function(value32) {
                return function(value42) {
                  return function(value52) {
                    return new ThreeLeft2(value0, value1, value22, value32, value42, value52);
                  };
                };
              };
            };
          };
        };
        return ThreeLeft2;
      }();
      ThreeMiddle = /* @__PURE__ */ function() {
        function ThreeMiddle2(value0, value1, value22, value32, value42, value52) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value22;
          this.value3 = value32;
          this.value4 = value42;
          this.value5 = value52;
        }
        ;
        ThreeMiddle2.create = function(value0) {
          return function(value1) {
            return function(value22) {
              return function(value32) {
                return function(value42) {
                  return function(value52) {
                    return new ThreeMiddle2(value0, value1, value22, value32, value42, value52);
                  };
                };
              };
            };
          };
        };
        return ThreeMiddle2;
      }();
      ThreeRight = /* @__PURE__ */ function() {
        function ThreeRight2(value0, value1, value22, value32, value42, value52) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value22;
          this.value3 = value32;
          this.value4 = value42;
          this.value5 = value52;
        }
        ;
        ThreeRight2.create = function(value0) {
          return function(value1) {
            return function(value22) {
              return function(value32) {
                return function(value42) {
                  return function(value52) {
                    return new ThreeRight2(value0, value1, value22, value32, value42, value52);
                  };
                };
              };
            };
          };
        };
        return ThreeRight2;
      }();
      KickUp = /* @__PURE__ */ function() {
        function KickUp2(value0, value1, value22, value32) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value22;
          this.value3 = value32;
        }
        ;
        KickUp2.create = function(value0) {
          return function(value1) {
            return function(value22) {
              return function(value32) {
                return new KickUp2(value0, value1, value22, value32);
              };
            };
          };
        };
        return KickUp2;
      }();
      lookup = function(dictOrd) {
        var compare2 = compare(dictOrd);
        return function(k) {
          var go2 = function($copy_v) {
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v) {
              if (v instanceof Leaf) {
                $tco_done = true;
                return Nothing.value;
              }
              ;
              if (v instanceof Two) {
                var v2 = compare2(k)(v.value1);
                if (v2 instanceof EQ) {
                  $tco_done = true;
                  return new Just(v.value2);
                }
                ;
                if (v2 instanceof LT) {
                  $copy_v = v.value0;
                  return;
                }
                ;
                $copy_v = v.value3;
                return;
              }
              ;
              if (v instanceof Three) {
                var v3 = compare2(k)(v.value1);
                if (v3 instanceof EQ) {
                  $tco_done = true;
                  return new Just(v.value2);
                }
                ;
                var v4 = compare2(k)(v.value4);
                if (v4 instanceof EQ) {
                  $tco_done = true;
                  return new Just(v.value5);
                }
                ;
                if (v3 instanceof LT) {
                  $copy_v = v.value0;
                  return;
                }
                ;
                if (v4 instanceof GT) {
                  $copy_v = v.value6;
                  return;
                }
                ;
                $copy_v = v.value3;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($copy_v);
            }
            ;
            return $tco_result;
          };
          return go2;
        };
      };
      fromZipper = function($copy_dictOrd) {
        return function($copy_v) {
          return function($copy_v1) {
            var $tco_var_dictOrd = $copy_dictOrd;
            var $tco_var_v = $copy_v;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(dictOrd, v, v1) {
              if (v instanceof Nil) {
                $tco_done = true;
                return v1;
              }
              ;
              if (v instanceof Cons) {
                if (v.value0 instanceof TwoLeft) {
                  $tco_var_dictOrd = dictOrd;
                  $tco_var_v = v.value1;
                  $copy_v1 = new Two(v1, v.value0.value0, v.value0.value1, v.value0.value2);
                  return;
                }
                ;
                if (v.value0 instanceof TwoRight) {
                  $tco_var_dictOrd = dictOrd;
                  $tco_var_v = v.value1;
                  $copy_v1 = new Two(v.value0.value0, v.value0.value1, v.value0.value2, v1);
                  return;
                }
                ;
                if (v.value0 instanceof ThreeLeft) {
                  $tco_var_dictOrd = dictOrd;
                  $tco_var_v = v.value1;
                  $copy_v1 = new Three(v1, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
                  return;
                }
                ;
                if (v.value0 instanceof ThreeMiddle) {
                  $tco_var_dictOrd = dictOrd;
                  $tco_var_v = v.value1;
                  $copy_v1 = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v1, v.value0.value3, v.value0.value4, v.value0.value5);
                  return;
                }
                ;
                if (v.value0 instanceof ThreeRight) {
                  $tco_var_dictOrd = dictOrd;
                  $tco_var_v = v.value1;
                  $copy_v1 = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, v1);
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): " + [v.value0.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): " + [v.constructor.name, v1.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_v1);
            }
            ;
            return $tco_result;
          };
        };
      };
      insert = function(dictOrd) {
        var fromZipper1 = fromZipper(dictOrd);
        var compare2 = compare(dictOrd);
        return function(k) {
          return function(v) {
            var up = function($copy_v1) {
              return function($copy_v2) {
                var $tco_var_v1 = $copy_v1;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(v1, v2) {
                  if (v1 instanceof Nil) {
                    $tco_done = true;
                    return new Two(v2.value0, v2.value1, v2.value2, v2.value3);
                  }
                  ;
                  if (v1 instanceof Cons) {
                    if (v1.value0 instanceof TwoLeft) {
                      $tco_done = true;
                      return fromZipper1(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
                    }
                    ;
                    if (v1.value0 instanceof TwoRight) {
                      $tco_done = true;
                      return fromZipper1(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
                    }
                    ;
                    if (v1.value0 instanceof ThreeLeft) {
                      $tco_var_v1 = v1.value1;
                      $copy_v2 = new KickUp(new Two(v2.value0, v2.value1, v2.value2, v2.value3), v1.value0.value0, v1.value0.value1, new Two(v1.value0.value2, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                      return;
                    }
                    ;
                    if (v1.value0 instanceof ThreeMiddle) {
                      $tco_var_v1 = v1.value1;
                      $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0), v2.value1, v2.value2, new Two(v2.value3, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                      return;
                    }
                    ;
                    if (v1.value0 instanceof ThreeRight) {
                      $tco_var_v1 = v1.value1;
                      $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v1.value0.value3), v1.value0.value4, v1.value0.value5, new Two(v2.value0, v2.value1, v2.value2, v2.value3));
                      return;
                    }
                    ;
                    throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): " + [v1.value0.constructor.name, v2.constructor.name]);
                  }
                  ;
                  throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): " + [v1.constructor.name, v2.constructor.name]);
                }
                ;
                while (!$tco_done) {
                  $tco_result = $tco_loop($tco_var_v1, $copy_v2);
                }
                ;
                return $tco_result;
              };
            };
            var down = function($copy_v1) {
              return function($copy_v2) {
                var $tco_var_v1 = $copy_v1;
                var $tco_done1 = false;
                var $tco_result;
                function $tco_loop(v1, v2) {
                  if (v2 instanceof Leaf) {
                    $tco_done1 = true;
                    return up(v1)(new KickUp(Leaf.value, k, v, Leaf.value));
                  }
                  ;
                  if (v2 instanceof Two) {
                    var v3 = compare2(k)(v2.value1);
                    if (v3 instanceof EQ) {
                      $tco_done1 = true;
                      return fromZipper1(v1)(new Two(v2.value0, k, v, v2.value3));
                    }
                    ;
                    if (v3 instanceof LT) {
                      $tco_var_v1 = new Cons(new TwoLeft(v2.value1, v2.value2, v2.value3), v1);
                      $copy_v2 = v2.value0;
                      return;
                    }
                    ;
                    $tco_var_v1 = new Cons(new TwoRight(v2.value0, v2.value1, v2.value2), v1);
                    $copy_v2 = v2.value3;
                    return;
                  }
                  ;
                  if (v2 instanceof Three) {
                    var v3 = compare2(k)(v2.value1);
                    if (v3 instanceof EQ) {
                      $tco_done1 = true;
                      return fromZipper1(v1)(new Three(v2.value0, k, v, v2.value3, v2.value4, v2.value5, v2.value6));
                    }
                    ;
                    var v4 = compare2(k)(v2.value4);
                    if (v4 instanceof EQ) {
                      $tco_done1 = true;
                      return fromZipper1(v1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, k, v, v2.value6));
                    }
                    ;
                    if (v3 instanceof LT) {
                      $tco_var_v1 = new Cons(new ThreeLeft(v2.value1, v2.value2, v2.value3, v2.value4, v2.value5, v2.value6), v1);
                      $copy_v2 = v2.value0;
                      return;
                    }
                    ;
                    if (v3 instanceof GT && v4 instanceof LT) {
                      $tco_var_v1 = new Cons(new ThreeMiddle(v2.value0, v2.value1, v2.value2, v2.value4, v2.value5, v2.value6), v1);
                      $copy_v2 = v2.value3;
                      return;
                    }
                    ;
                    $tco_var_v1 = new Cons(new ThreeRight(v2.value0, v2.value1, v2.value2, v2.value3, v2.value4, v2.value5), v1);
                    $copy_v2 = v2.value6;
                    return;
                  }
                  ;
                  throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): " + [v1.constructor.name, v2.constructor.name]);
                }
                ;
                while (!$tco_done1) {
                  $tco_result = $tco_loop($tco_var_v1, $copy_v2);
                }
                ;
                return $tco_result;
              };
            };
            return down(Nil.value);
          };
        };
      };
      pop = function(dictOrd) {
        var fromZipper1 = fromZipper(dictOrd);
        var compare2 = compare(dictOrd);
        return function(k) {
          var up = function($copy_ctxs) {
            return function($copy_tree) {
              var $tco_var_ctxs = $copy_ctxs;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(ctxs, tree) {
                if (ctxs instanceof Nil) {
                  $tco_done = true;
                  return tree;
                }
                ;
                if (ctxs instanceof Cons) {
                  if (ctxs.value0 instanceof TwoLeft && (ctxs.value0.value2 instanceof Leaf && tree instanceof Leaf)) {
                    $tco_done = true;
                    return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value));
                  }
                  ;
                  if (ctxs.value0 instanceof TwoRight && (ctxs.value0.value0 instanceof Leaf && tree instanceof Leaf)) {
                    $tco_done = true;
                    return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value));
                  }
                  ;
                  if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Two) {
                    $tco_var_ctxs = ctxs.value1;
                    $copy_tree = new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3);
                    return;
                  }
                  ;
                  if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Two) {
                    $tco_var_ctxs = ctxs.value1;
                    $copy_tree = new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree);
                    return;
                  }
                  ;
                  if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Three) {
                    $tco_done = true;
                    return fromZipper1(ctxs.value1)(new Two(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6)));
                  }
                  ;
                  if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Three) {
                    $tco_done = true;
                    return fromZipper1(ctxs.value1)(new Two(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree)));
                  }
                  ;
                  if (ctxs.value0 instanceof ThreeLeft && (ctxs.value0.value2 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                    $tco_done = true;
                    return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
                  }
                  ;
                  if (ctxs.value0 instanceof ThreeMiddle && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                    $tco_done = true;
                    return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
                  }
                  ;
                  if (ctxs.value0 instanceof ThreeRight && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value3 instanceof Leaf && tree instanceof Leaf))) {
                    $tco_done = true;
                    return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value4, ctxs.value0.value5, Leaf.value));
                  }
                  ;
                  if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Two) {
                    $tco_done = true;
                    return fromZipper1(ctxs.value1)(new Two(new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
                  }
                  ;
                  if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Two) {
                    $tco_done = true;
                    return fromZipper1(ctxs.value1)(new Two(new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
                  }
                  ;
                  if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Two) {
                    $tco_done = true;
                    return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0, ctxs.value0.value5.value1, ctxs.value0.value5.value2, ctxs.value0.value5.value3)));
                  }
                  ;
                  if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Two) {
                    $tco_done = true;
                    return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3, ctxs.value0.value4, ctxs.value0.value5, tree)));
                  }
                  ;
                  if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Three) {
                    $tco_done = true;
                    return fromZipper1(ctxs.value1)(new Three(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
                  }
                  ;
                  if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Three) {
                    $tco_done = true;
                    return fromZipper1(ctxs.value1)(new Three(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
                  }
                  ;
                  if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Three) {
                    $tco_done = true;
                    return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0), ctxs.value0.value5.value1, ctxs.value0.value5.value2, new Two(ctxs.value0.value5.value3, ctxs.value0.value5.value4, ctxs.value0.value5.value5, ctxs.value0.value5.value6)));
                  }
                  ;
                  if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Three) {
                    $tco_done = true;
                    return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3), ctxs.value0.value3.value4, ctxs.value0.value3.value5, new Two(ctxs.value0.value3.value6, ctxs.value0.value4, ctxs.value0.value5, tree)));
                  }
                  ;
                  $tco_done = true;
                  return unsafeCrashWith("The impossible happened in partial function `up`.");
                }
                ;
                throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): " + [ctxs.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_ctxs, $copy_tree);
              }
              ;
              return $tco_result;
            };
          };
          var removeMaxNode = function($copy_ctx) {
            return function($copy_m) {
              var $tco_var_ctx = $copy_ctx;
              var $tco_done1 = false;
              var $tco_result;
              function $tco_loop(ctx, m) {
                if (m instanceof Two && (m.value0 instanceof Leaf && m.value3 instanceof Leaf)) {
                  $tco_done1 = true;
                  return up(ctx)(Leaf.value);
                }
                ;
                if (m instanceof Two) {
                  $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
                  $copy_m = m.value3;
                  return;
                }
                ;
                if (m instanceof Three && (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf))) {
                  $tco_done1 = true;
                  return up(new Cons(new TwoRight(Leaf.value, m.value1, m.value2), ctx))(Leaf.value);
                }
                ;
                if (m instanceof Three) {
                  $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
                  $copy_m = m.value6;
                  return;
                }
                ;
                $tco_done1 = true;
                return unsafeCrashWith("The impossible happened in partial function `removeMaxNode`.");
              }
              ;
              while (!$tco_done1) {
                $tco_result = $tco_loop($tco_var_ctx, $copy_m);
              }
              ;
              return $tco_result;
            };
          };
          var maxNode = function($copy_m) {
            var $tco_done2 = false;
            var $tco_result;
            function $tco_loop(m) {
              if (m instanceof Two && m.value3 instanceof Leaf) {
                $tco_done2 = true;
                return {
                  key: m.value1,
                  value: m.value2
                };
              }
              ;
              if (m instanceof Two) {
                $copy_m = m.value3;
                return;
              }
              ;
              if (m instanceof Three && m.value6 instanceof Leaf) {
                $tco_done2 = true;
                return {
                  key: m.value4,
                  value: m.value5
                };
              }
              ;
              if (m instanceof Three) {
                $copy_m = m.value6;
                return;
              }
              ;
              $tco_done2 = true;
              return unsafeCrashWith("The impossible happened in partial function `maxNode`.");
            }
            ;
            while (!$tco_done2) {
              $tco_result = $tco_loop($copy_m);
            }
            ;
            return $tco_result;
          };
          var down = function($copy_ctx) {
            return function($copy_m) {
              var $tco_var_ctx = $copy_ctx;
              var $tco_done3 = false;
              var $tco_result;
              function $tco_loop(ctx, m) {
                if (m instanceof Leaf) {
                  $tco_done3 = true;
                  return Nothing.value;
                }
                ;
                if (m instanceof Two) {
                  var v = compare2(k)(m.value1);
                  if (m.value3 instanceof Leaf && v instanceof EQ) {
                    $tco_done3 = true;
                    return new Just(new Tuple(m.value2, up(ctx)(Leaf.value)));
                  }
                  ;
                  if (v instanceof EQ) {
                    var max6 = maxNode(m.value0);
                    $tco_done3 = true;
                    return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new TwoLeft(max6.key, max6.value, m.value3), ctx))(m.value0)));
                  }
                  ;
                  if (v instanceof LT) {
                    $tco_var_ctx = new Cons(new TwoLeft(m.value1, m.value2, m.value3), ctx);
                    $copy_m = m.value0;
                    return;
                  }
                  ;
                  $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
                  $copy_m = m.value3;
                  return;
                }
                ;
                if (m instanceof Three) {
                  var leaves = function() {
                    if (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf)) {
                      return true;
                    }
                    ;
                    return false;
                  }();
                  var v = compare2(k)(m.value4);
                  var v3 = compare2(k)(m.value1);
                  if (leaves && v3 instanceof EQ) {
                    $tco_done3 = true;
                    return new Just(new Tuple(m.value2, fromZipper1(ctx)(new Two(Leaf.value, m.value4, m.value5, Leaf.value))));
                  }
                  ;
                  if (leaves && v instanceof EQ) {
                    $tco_done3 = true;
                    return new Just(new Tuple(m.value5, fromZipper1(ctx)(new Two(Leaf.value, m.value1, m.value2, Leaf.value))));
                  }
                  ;
                  if (v3 instanceof EQ) {
                    var max6 = maxNode(m.value0);
                    $tco_done3 = true;
                    return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new ThreeLeft(max6.key, max6.value, m.value3, m.value4, m.value5, m.value6), ctx))(m.value0)));
                  }
                  ;
                  if (v instanceof EQ) {
                    var max6 = maxNode(m.value3);
                    $tco_done3 = true;
                    return new Just(new Tuple(m.value5, removeMaxNode(new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, max6.key, max6.value, m.value6), ctx))(m.value3)));
                  }
                  ;
                  if (v3 instanceof LT) {
                    $tco_var_ctx = new Cons(new ThreeLeft(m.value1, m.value2, m.value3, m.value4, m.value5, m.value6), ctx);
                    $copy_m = m.value0;
                    return;
                  }
                  ;
                  if (v3 instanceof GT && v instanceof LT) {
                    $tco_var_ctx = new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, m.value4, m.value5, m.value6), ctx);
                    $copy_m = m.value3;
                    return;
                  }
                  ;
                  $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
                  $copy_m = m.value6;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): " + [m.constructor.name]);
              }
              ;
              while (!$tco_done3) {
                $tco_result = $tco_loop($tco_var_ctx, $copy_m);
              }
              ;
              return $tco_result;
            };
          };
          return down(Nil.value);
        };
      };
      foldableMap = {
        foldr: function(f) {
          return function(z) {
            return function(m) {
              if (m instanceof Leaf) {
                return z;
              }
              ;
              if (m instanceof Two) {
                return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(z)(m.value3)))(m.value0);
              }
              ;
              if (m instanceof Three) {
                return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(f(m.value5)(foldr(foldableMap)(f)(z)(m.value6)))(m.value3)))(m.value0);
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): " + [m.constructor.name]);
            };
          };
        },
        foldl: function(f) {
          return function(z) {
            return function(m) {
              if (m instanceof Leaf) {
                return z;
              }
              ;
              if (m instanceof Two) {
                return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3);
              }
              ;
              if (m instanceof Three) {
                return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3))(m.value5))(m.value6);
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): " + [m.constructor.name]);
            };
          };
        },
        foldMap: function(dictMonoid) {
          var mempty2 = mempty(dictMonoid);
          var append22 = append(dictMonoid.Semigroup0());
          return function(f) {
            return function(m) {
              if (m instanceof Leaf) {
                return mempty2;
              }
              ;
              if (m instanceof Two) {
                return append22(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append22(f(m.value2))(foldMap(foldableMap)(dictMonoid)(f)(m.value3)));
              }
              ;
              if (m instanceof Three) {
                return append22(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append22(f(m.value2))(append22(foldMap(foldableMap)(dictMonoid)(f)(m.value3))(append22(f(m.value5))(foldMap(foldableMap)(dictMonoid)(f)(m.value6)))));
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): " + [m.constructor.name]);
            };
          };
        }
      };
      empty2 = /* @__PURE__ */ function() {
        return Leaf.value;
      }();
      $$delete = function(dictOrd) {
        var pop1 = pop(dictOrd);
        return function(k) {
          return function(m) {
            return maybe(m)(snd)(pop1(k)(m));
          };
        };
      };
      alter = function(dictOrd) {
        var lookup12 = lookup(dictOrd);
        var delete1 = $$delete(dictOrd);
        var insert12 = insert(dictOrd);
        return function(f) {
          return function(k) {
            return function(m) {
              var v = f(lookup12(k)(m));
              if (v instanceof Nothing) {
                return delete1(k)(m);
              }
              ;
              if (v instanceof Just) {
                return insert12(k)(v.value0)(m);
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): " + [v.constructor.name]);
            };
          };
        };
      };
    }
  });

  // output/Data.Monoid.Alternate/index.js
  var init_Data_Monoid7 = __esm({
    "output/Data.Monoid.Alternate/index.js"() {
      init_Control7();
      init_Control9();
      init_Data14();
    }
  });

  // output/Halogen.Data.OrdBox/index.js
  var init_Halogen_Data = __esm({
    "output/Halogen.Data.OrdBox/index.js"() {
      init_Data8();
      init_Data12();
    }
  });

  // output/Halogen.Data.Slot/index.js
  var foreachSlot, empty3;
  var init_Halogen_Data2 = __esm({
    "output/Halogen.Data.Slot/index.js"() {
      init_Data();
      init_Data26();
      init_Data_Map();
      init_Data15();
      init_Data_Monoid7();
      init_Data25();
      init_Data12();
      init_Data5();
      init_Data22();
      init_Halogen_Data();
      foreachSlot = function(dictApplicative) {
        var traverse_7 = traverse_(dictApplicative)(foldableMap);
        return function(v) {
          return function(k) {
            return traverse_7(function($54) {
              return k($54);
            })(v);
          };
        };
      };
      empty3 = empty2;
    }
  });

  // output/DOM.HTML.Indexed.AutocompleteType/index.js
  var init_DOM_HTML_Indexed3 = __esm({
    "output/DOM.HTML.Indexed.AutocompleteType/index.js"() {
      init_Data9();
    }
  });

  // output/DOM.HTML.Indexed.ButtonType/index.js
  var init_DOM_HTML_Indexed4 = __esm({
    "output/DOM.HTML.Indexed.ButtonType/index.js"() {
      init_Data9();
    }
  });

  // output/DOM.HTML.Indexed.CrossOriginValue/index.js
  var init_DOM_HTML_Indexed5 = __esm({
    "output/DOM.HTML.Indexed.CrossOriginValue/index.js"() {
      init_Data9();
    }
  });

  // output/DOM.HTML.Indexed.DirValue/index.js
  var init_DOM_HTML_Indexed6 = __esm({
    "output/DOM.HTML.Indexed.DirValue/index.js"() {
      init_Data9();
    }
  });

  // output/DOM.HTML.Indexed.FormMethod/index.js
  var init_DOM_HTML_Indexed7 = __esm({
    "output/DOM.HTML.Indexed.FormMethod/index.js"() {
      init_Data9();
    }
  });

  // output/DOM.HTML.Indexed.KindValue/index.js
  var init_DOM_HTML_Indexed8 = __esm({
    "output/DOM.HTML.Indexed.KindValue/index.js"() {
      init_Data9();
    }
  });

  // output/DOM.HTML.Indexed.MenuType/index.js
  var init_DOM_HTML_Indexed9 = __esm({
    "output/DOM.HTML.Indexed.MenuType/index.js"() {
      init_Data9();
    }
  });

  // output/DOM.HTML.Indexed.MenuitemType/index.js
  var init_DOM_HTML_Indexed10 = __esm({
    "output/DOM.HTML.Indexed.MenuitemType/index.js"() {
      init_Data9();
    }
  });

  // output/DOM.HTML.Indexed.OrderedListType/index.js
  var init_DOM_HTML_Indexed11 = __esm({
    "output/DOM.HTML.Indexed.OrderedListType/index.js"() {
      init_Data8();
      init_Data12();
      init_Data9();
    }
  });

  // output/DOM.HTML.Indexed.PreloadValue/index.js
  var init_DOM_HTML_Indexed12 = __esm({
    "output/DOM.HTML.Indexed.PreloadValue/index.js"() {
      init_Data9();
    }
  });

  // output/DOM.HTML.Indexed.ScopeValue/index.js
  var init_DOM_HTML_Indexed13 = __esm({
    "output/DOM.HTML.Indexed.ScopeValue/index.js"() {
      init_Data9();
    }
  });

  // output/DOM.HTML.Indexed.StepValue/index.js
  var init_DOM_HTML_Indexed14 = __esm({
    "output/DOM.HTML.Indexed.StepValue/index.js"() {
      init_Data12();
      init_Data9();
      init_Data14();
    }
  });

  // output/DOM.HTML.Indexed.WrapValue/index.js
  var init_DOM_HTML_Indexed15 = __esm({
    "output/DOM.HTML.Indexed.WrapValue/index.js"() {
      init_Data9();
    }
  });

  // output/Halogen.Query.Input/index.js
  var RefUpdate, Action;
  var init_Halogen_Query = __esm({
    "output/Halogen.Query.Input/index.js"() {
      init_Data8();
      init_Data12();
      RefUpdate = /* @__PURE__ */ function() {
        function RefUpdate2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        RefUpdate2.create = function(value0) {
          return function(value1) {
            return new RefUpdate2(value0, value1);
          };
        };
        return RefUpdate2;
      }();
      Action = /* @__PURE__ */ function() {
        function Action3(value0) {
          this.value0 = value0;
        }
        ;
        Action3.create = function(value0) {
          return new Action3(value0);
        };
        return Action3;
      }();
    }
  });

  // output/Halogen.VDom.Machine/index.js
  var Step, unStep, step3, mkStep, halt, extract2;
  var init_Halogen_VDom = __esm({
    "output/Halogen.VDom.Machine/index.js"() {
      init_Unsafe();
      Step = /* @__PURE__ */ function() {
        function Step3(value0, value1, value22, value32) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value22;
          this.value3 = value32;
        }
        ;
        Step3.create = function(value0) {
          return function(value1) {
            return function(value22) {
              return function(value32) {
                return new Step3(value0, value1, value22, value32);
              };
            };
          };
        };
        return Step3;
      }();
      unStep = unsafeCoerce2;
      step3 = function(v, a2) {
        return v.value2(v.value1, a2);
      };
      mkStep = unsafeCoerce2;
      halt = function(v) {
        return v.value3(v.value1);
      };
      extract2 = /* @__PURE__ */ unStep(function(v) {
        return v.value0;
      });
    }
  });

  // output/Halogen.VDom.Types/index.js
  var map10, map12, Text, Elem, Keyed, Widget, Grafted, Graft, unGraft, graft, bifunctorGraft, bimap3, runGraft;
  var init_Halogen_VDom2 = __esm({
    "output/Halogen.VDom.Types/index.js"() {
      init_Control2();
      init_Data24();
      init_Data8();
      init_Data4();
      init_Data12();
      init_Data22();
      init_Unsafe();
      map10 = /* @__PURE__ */ map(functorArray);
      map12 = /* @__PURE__ */ map(functorTuple);
      Text = /* @__PURE__ */ function() {
        function Text3(value0) {
          this.value0 = value0;
        }
        ;
        Text3.create = function(value0) {
          return new Text3(value0);
        };
        return Text3;
      }();
      Elem = /* @__PURE__ */ function() {
        function Elem2(value0, value1, value22, value32) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value22;
          this.value3 = value32;
        }
        ;
        Elem2.create = function(value0) {
          return function(value1) {
            return function(value22) {
              return function(value32) {
                return new Elem2(value0, value1, value22, value32);
              };
            };
          };
        };
        return Elem2;
      }();
      Keyed = /* @__PURE__ */ function() {
        function Keyed2(value0, value1, value22, value32) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value22;
          this.value3 = value32;
        }
        ;
        Keyed2.create = function(value0) {
          return function(value1) {
            return function(value22) {
              return function(value32) {
                return new Keyed2(value0, value1, value22, value32);
              };
            };
          };
        };
        return Keyed2;
      }();
      Widget = /* @__PURE__ */ function() {
        function Widget2(value0) {
          this.value0 = value0;
        }
        ;
        Widget2.create = function(value0) {
          return new Widget2(value0);
        };
        return Widget2;
      }();
      Grafted = /* @__PURE__ */ function() {
        function Grafted2(value0) {
          this.value0 = value0;
        }
        ;
        Grafted2.create = function(value0) {
          return new Grafted2(value0);
        };
        return Grafted2;
      }();
      Graft = /* @__PURE__ */ function() {
        function Graft2(value0, value1, value22) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value22;
        }
        ;
        Graft2.create = function(value0) {
          return function(value1) {
            return function(value22) {
              return new Graft2(value0, value1, value22);
            };
          };
        };
        return Graft2;
      }();
      unGraft = function(f) {
        return function($61) {
          return f($61);
        };
      };
      graft = unsafeCoerce2;
      bifunctorGraft = {
        bimap: function(f) {
          return function(g) {
            return unGraft(function(v) {
              return graft(new Graft(function($63) {
                return f(v.value0($63));
              }, function($64) {
                return g(v.value1($64));
              }, v.value2));
            });
          };
        }
      };
      bimap3 = /* @__PURE__ */ bimap(bifunctorGraft);
      runGraft = /* @__PURE__ */ unGraft(function(v) {
        var go2 = function(v2) {
          if (v2 instanceof Text) {
            return new Text(v2.value0);
          }
          ;
          if (v2 instanceof Elem) {
            return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map10(go2)(v2.value3));
          }
          ;
          if (v2 instanceof Keyed) {
            return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map10(map12(go2))(v2.value3));
          }
          ;
          if (v2 instanceof Widget) {
            return new Widget(v.value1(v2.value0));
          }
          ;
          if (v2 instanceof Grafted) {
            return new Grafted(bimap3(v.value0)(v.value1)(v2.value0));
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.Types (line 86, column 7 - line 86, column 27): " + [v2.constructor.name]);
        };
        return go2(v.value2);
      });
    }
  });

  // output/Halogen.VDom.Util/foreign.js
  function unsafeGetAny(key, obj) {
    return obj[key];
  }
  function unsafeHasAny(key, obj) {
    return obj.hasOwnProperty(key);
  }
  function unsafeSetAny(key, val, obj) {
    obj[key] = val;
  }
  function forE2(a2, f) {
    var b2 = [];
    for (var i2 = 0; i2 < a2.length; i2++) {
      b2.push(f(i2, a2[i2]));
    }
    return b2;
  }
  function forEachE(a2, f) {
    for (var i2 = 0; i2 < a2.length; i2++) {
      f(a2[i2]);
    }
  }
  function forInE(o, f) {
    var ks = Object.keys(o);
    for (var i2 = 0; i2 < ks.length; i2++) {
      var k = ks[i2];
      f(k, o[k]);
    }
  }
  function diffWithIxE(a1, a2, f1, f2, f3) {
    var a3 = [];
    var l1 = a1.length;
    var l2 = a2.length;
    var i2 = 0;
    while (1) {
      if (i2 < l1) {
        if (i2 < l2) {
          a3.push(f1(i2, a1[i2], a2[i2]));
        } else {
          f2(i2, a1[i2]);
        }
      } else if (i2 < l2) {
        a3.push(f3(i2, a2[i2]));
      } else {
        break;
      }
      i2++;
    }
    return a3;
  }
  function strMapWithIxE(as, fk, f) {
    var o = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      o[k] = f(k, i2, a2);
    }
    return o;
  }
  function diffWithKeyAndIxE(o1, as, fk, f1, f2, f3) {
    var o2 = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      if (o1.hasOwnProperty(k)) {
        o2[k] = f1(k, i2, o1[k], a2);
      } else {
        o2[k] = f3(k, i2, a2);
      }
    }
    for (var k in o1) {
      if (k in o2) {
        continue;
      }
      f2(k, o1[k]);
    }
    return o2;
  }
  function refEq2(a2, b2) {
    return a2 === b2;
  }
  function createTextNode(s, doc) {
    return doc.createTextNode(s);
  }
  function setTextContent(s, n) {
    n.textContent = s;
  }
  function createElement(ns, name15, doc) {
    if (ns != null) {
      return doc.createElementNS(ns, name15);
    } else {
      return doc.createElement(name15);
    }
  }
  function insertChildIx(i2, a2, b2) {
    var n = b2.childNodes.item(i2) || null;
    if (n !== a2) {
      b2.insertBefore(a2, n);
    }
  }
  function removeChild(a2, b2) {
    if (b2 && a2.parentNode === b2) {
      b2.removeChild(a2);
    }
  }
  function parentNode(a2) {
    return a2.parentNode;
  }
  function setAttribute(ns, attr3, val, el) {
    if (ns != null) {
      el.setAttributeNS(ns, attr3, val);
    } else {
      el.setAttribute(attr3, val);
    }
  }
  function removeAttribute(ns, attr3, el) {
    if (ns != null) {
      el.removeAttributeNS(ns, attr3);
    } else {
      el.removeAttribute(attr3);
    }
  }
  function hasAttribute(ns, attr3, el) {
    if (ns != null) {
      return el.hasAttributeNS(ns, attr3);
    } else {
      return el.hasAttribute(attr3);
    }
  }
  function addEventListener2(ev, listener, el) {
    el.addEventListener(ev, listener, false);
  }
  function removeEventListener2(ev, listener, el) {
    el.removeEventListener(ev, listener, false);
  }
  var jsUndefined;
  var init_foreign99 = __esm({
    "output/Halogen.VDom.Util/foreign.js"() {
      "use strict";
      jsUndefined = void 0;
    }
  });

  // output/Foreign.Object.ST/foreign.js
  var newImpl;
  var init_foreign100 = __esm({
    "output/Foreign.Object.ST/foreign.js"() {
      newImpl = function() {
        return {};
      };
    }
  });

  // output/Foreign.Object.ST/index.js
  var init_Foreign_Object = __esm({
    "output/Foreign.Object.ST/index.js"() {
      init_foreign100();
      init_Data15();
      init_foreign100();
    }
  });

  // output/Halogen.VDom.Util/index.js
  var unsafeLookup, unsafeFreeze2, pokeMutMap, newMutMap;
  var init_Halogen_VDom3 = __esm({
    "output/Halogen.VDom.Util/index.js"() {
      init_foreign99();
      init_Foreign_Object();
      init_Unsafe();
      init_foreign99();
      unsafeLookup = unsafeGetAny;
      unsafeFreeze2 = unsafeCoerce2;
      pokeMutMap = unsafeSetAny;
      newMutMap = newImpl;
    }
  });

  // output/Web.DOM.Element/foreign.js
  var getProp, _namespaceURI, _prefix, localName, tagName;
  var init_foreign101 = __esm({
    "output/Web.DOM.Element/foreign.js"() {
      getProp = function(name15) {
        return function(doctype) {
          return doctype[name15];
        };
      };
      _namespaceURI = getProp("namespaceURI");
      _prefix = getProp("prefix");
      localName = getProp("localName");
      tagName = getProp("tagName");
    }
  });

  // output/Web.DOM.ShadowRoot/foreign.js
  var init_foreign102 = __esm({
    "output/Web.DOM.ShadowRoot/foreign.js"() {
    }
  });

  // output/Web.DOM.ShadowRoot/index.js
  var init_Web_DOM2 = __esm({
    "output/Web.DOM.ShadowRoot/index.js"() {
      init_foreign102();
      init_Data15();
      init_Unsafe();
      init_foreign102();
    }
  });

  // output/Web.DOM.Element/index.js
  var toNode2;
  var init_Web_DOM3 = __esm({
    "output/Web.DOM.Element/index.js"() {
      init_foreign101();
      init_Data4();
      init_Data36();
      init_Data14();
      init_Effect();
      init_Unsafe();
      init_Web_DOM();
      init_Web_DOM2();
      init_Web_Internal();
      init_foreign101();
      toNode2 = unsafeCoerce2;
    }
  });

  // output/Halogen.VDom.DOM/index.js
  var $runtime_lazy3, haltWidget, $lazy_patchWidget, patchWidget, haltText, $lazy_patchText, patchText, haltKeyed, haltElem, eqElemSpec, $lazy_patchElem, patchElem, $lazy_patchKeyed, patchKeyed, buildWidget, buildText, buildKeyed, buildElem, buildVDom;
  var init_Halogen_VDom4 = __esm({
    "output/Halogen.VDom.DOM/index.js"() {
      init_Data32();
      init_Data();
      init_Data15();
      init_Data36();
      init_Data22();
      init_Halogen_VDom();
      init_Halogen_VDom2();
      init_Halogen_VDom3();
      init_Web_DOM3();
      $runtime_lazy3 = function(name15, moduleName, init2) {
        var state3 = 0;
        var val;
        return function(lineNumber) {
          if (state3 === 2)
            return val;
          if (state3 === 1)
            throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
          state3 = 1;
          val = init2();
          state3 = 2;
          return val;
        };
      };
      haltWidget = function(v) {
        return halt(v.widget);
      };
      $lazy_patchWidget = /* @__PURE__ */ $runtime_lazy3("patchWidget", "Halogen.VDom.DOM", function() {
        return function(state3, vdom) {
          if (vdom instanceof Grafted) {
            return $lazy_patchWidget(291)(state3, runGraft(vdom.value0));
          }
          ;
          if (vdom instanceof Widget) {
            var res = step3(state3.widget, vdom.value0);
            var res$prime = unStep(function(v) {
              return mkStep(new Step(v.value0, {
                build: state3.build,
                widget: res
              }, $lazy_patchWidget(296), haltWidget));
            })(res);
            return res$prime;
          }
          ;
          haltWidget(state3);
          return state3.build(vdom);
        };
      });
      patchWidget = /* @__PURE__ */ $lazy_patchWidget(286);
      haltText = function(v) {
        var parent2 = parentNode(v.node);
        return removeChild(v.node, parent2);
      };
      $lazy_patchText = /* @__PURE__ */ $runtime_lazy3("patchText", "Halogen.VDom.DOM", function() {
        return function(state3, vdom) {
          if (vdom instanceof Grafted) {
            return $lazy_patchText(82)(state3, runGraft(vdom.value0));
          }
          ;
          if (vdom instanceof Text) {
            if (state3.value === vdom.value0) {
              return mkStep(new Step(state3.node, state3, $lazy_patchText(85), haltText));
            }
            ;
            if (otherwise) {
              var nextState = {
                build: state3.build,
                node: state3.node,
                value: vdom.value0
              };
              setTextContent(vdom.value0, state3.node);
              return mkStep(new Step(state3.node, nextState, $lazy_patchText(89), haltText));
            }
            ;
          }
          ;
          haltText(state3);
          return state3.build(vdom);
        };
      });
      patchText = /* @__PURE__ */ $lazy_patchText(77);
      haltKeyed = function(v) {
        var parent2 = parentNode(v.node);
        removeChild(v.node, parent2);
        forInE(v.children, function(v1, s) {
          return halt(s);
        });
        return halt(v.attrs);
      };
      haltElem = function(v) {
        var parent2 = parentNode(v.node);
        removeChild(v.node, parent2);
        forEachE(v.children, halt);
        return halt(v.attrs);
      };
      eqElemSpec = function(ns1, v, ns2, v1) {
        var $63 = v === v1;
        if ($63) {
          if (ns1 instanceof Just && (ns2 instanceof Just && ns1.value0 === ns2.value0)) {
            return true;
          }
          ;
          if (ns1 instanceof Nothing && ns2 instanceof Nothing) {
            return true;
          }
          ;
          return false;
        }
        ;
        return false;
      };
      $lazy_patchElem = /* @__PURE__ */ $runtime_lazy3("patchElem", "Halogen.VDom.DOM", function() {
        return function(state3, vdom) {
          if (vdom instanceof Grafted) {
            return $lazy_patchElem(135)(state3, runGraft(vdom.value0));
          }
          ;
          if (vdom instanceof Elem && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
            var v = length(vdom.value3);
            var v1 = length(state3.children);
            if (v1 === 0 && v === 0) {
              var attrs2 = step3(state3.attrs, vdom.value2);
              var nextState = {
                build: state3.build,
                node: state3.node,
                attrs: attrs2,
                ns: vdom.value0,
                name: vdom.value1,
                children: state3.children
              };
              return mkStep(new Step(state3.node, nextState, $lazy_patchElem(149), haltElem));
            }
            ;
            var onThis = function(v2, s) {
              return halt(s);
            };
            var onThese = function(ix, s, v2) {
              var res = step3(s, v2);
              insertChildIx(ix, extract2(res), state3.node);
              return res;
            };
            var onThat = function(ix, v2) {
              var res = state3.build(v2);
              insertChildIx(ix, extract2(res), state3.node);
              return res;
            };
            var children2 = diffWithIxE(state3.children, vdom.value3, onThese, onThis, onThat);
            var attrs2 = step3(state3.attrs, vdom.value2);
            var nextState = {
              build: state3.build,
              node: state3.node,
              attrs: attrs2,
              ns: vdom.value0,
              name: vdom.value1,
              children: children2
            };
            return mkStep(new Step(state3.node, nextState, $lazy_patchElem(172), haltElem));
          }
          ;
          haltElem(state3);
          return state3.build(vdom);
        };
      });
      patchElem = /* @__PURE__ */ $lazy_patchElem(130);
      $lazy_patchKeyed = /* @__PURE__ */ $runtime_lazy3("patchKeyed", "Halogen.VDom.DOM", function() {
        return function(state3, vdom) {
          if (vdom instanceof Grafted) {
            return $lazy_patchKeyed(222)(state3, runGraft(vdom.value0));
          }
          ;
          if (vdom instanceof Keyed && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
            var v = length(vdom.value3);
            if (state3.length === 0 && v === 0) {
              var attrs2 = step3(state3.attrs, vdom.value2);
              var nextState = {
                build: state3.build,
                node: state3.node,
                attrs: attrs2,
                ns: vdom.value0,
                name: vdom.value1,
                children: state3.children,
                length: 0
              };
              return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(237), haltKeyed));
            }
            ;
            var onThis = function(v2, s) {
              return halt(s);
            };
            var onThese = function(v2, ix$prime, s, v3) {
              var res = step3(s, v3.value1);
              insertChildIx(ix$prime, extract2(res), state3.node);
              return res;
            };
            var onThat = function(v2, ix, v3) {
              var res = state3.build(v3.value1);
              insertChildIx(ix, extract2(res), state3.node);
              return res;
            };
            var children2 = diffWithKeyAndIxE(state3.children, vdom.value3, fst, onThese, onThis, onThat);
            var attrs2 = step3(state3.attrs, vdom.value2);
            var nextState = {
              build: state3.build,
              node: state3.node,
              attrs: attrs2,
              ns: vdom.value0,
              name: vdom.value1,
              children: children2,
              length: v
            };
            return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(261), haltKeyed));
          }
          ;
          haltKeyed(state3);
          return state3.build(vdom);
        };
      });
      patchKeyed = /* @__PURE__ */ $lazy_patchKeyed(217);
      buildWidget = function(v, build, w) {
        var res = v.buildWidget(v)(w);
        var res$prime = unStep(function(v1) {
          return mkStep(new Step(v1.value0, {
            build,
            widget: res
          }, patchWidget, haltWidget));
        })(res);
        return res$prime;
      };
      buildText = function(v, build, s) {
        var node = createTextNode(s, v.document);
        var state3 = {
          build,
          node,
          value: s
        };
        return mkStep(new Step(node, state3, patchText, haltText));
      };
      buildKeyed = function(v, build, ns1, name1, as1, ch1) {
        var el = createElement(toNullable(ns1), name1, v.document);
        var node = toNode2(el);
        var onChild = function(v1, ix, v2) {
          var res = build(v2.value1);
          insertChildIx(ix, extract2(res), node);
          return res;
        };
        var children2 = strMapWithIxE(ch1, fst, onChild);
        var attrs = v.buildAttributes(el)(as1);
        var state3 = {
          build,
          node,
          attrs,
          ns: ns1,
          name: name1,
          children: children2,
          length: length(ch1)
        };
        return mkStep(new Step(node, state3, patchKeyed, haltKeyed));
      };
      buildElem = function(v, build, ns1, name1, as1, ch1) {
        var el = createElement(toNullable(ns1), name1, v.document);
        var node = toNode2(el);
        var onChild = function(ix, child) {
          var res = build(child);
          insertChildIx(ix, extract2(res), node);
          return res;
        };
        var children2 = forE2(ch1, onChild);
        var attrs = v.buildAttributes(el)(as1);
        var state3 = {
          build,
          node,
          attrs,
          ns: ns1,
          name: name1,
          children: children2
        };
        return mkStep(new Step(node, state3, patchElem, haltElem));
      };
      buildVDom = function(spec) {
        var $lazy_build = $runtime_lazy3("build", "Halogen.VDom.DOM", function() {
          return function(v) {
            if (v instanceof Text) {
              return buildText(spec, $lazy_build(59), v.value0);
            }
            ;
            if (v instanceof Elem) {
              return buildElem(spec, $lazy_build(60), v.value0, v.value1, v.value2, v.value3);
            }
            ;
            if (v instanceof Keyed) {
              return buildKeyed(spec, $lazy_build(61), v.value0, v.value1, v.value2, v.value3);
            }
            ;
            if (v instanceof Widget) {
              return buildWidget(spec, $lazy_build(62), v.value0);
            }
            ;
            if (v instanceof Grafted) {
              return $lazy_build(63)(runGraft(v.value0));
            }
            ;
            throw new Error("Failed pattern match at Halogen.VDom.DOM (line 58, column 27 - line 63, column 52): " + [v.constructor.name]);
          };
        });
        var build = $lazy_build(58);
        return build;
      };
    }
  });

  // output/Halogen.VDom/index.js
  var init_Halogen = __esm({
    "output/Halogen.VDom/index.js"() {
      init_Halogen_VDom4();
      init_Halogen_VDom();
      init_Halogen_VDom2();
      init_Halogen_VDom4();
      init_Halogen_VDom();
      init_Halogen_VDom2();
    }
  });

  // output/Foreign/foreign.js
  function typeOf(value12) {
    return typeof value12;
  }
  var isArray;
  var init_foreign103 = __esm({
    "output/Foreign/foreign.js"() {
      isArray = Array.isArray || function(value12) {
        return Object.prototype.toString.call(value12) === "[object Array]";
      };
    }
  });

  // output/Data.Int/foreign.js
  var fromNumberImpl, toNumber;
  var init_foreign104 = __esm({
    "output/Data.Int/foreign.js"() {
      fromNumberImpl = function(just) {
        return function(nothing) {
          return function(n) {
            return (n | 0) === n ? just(n) : nothing;
          };
        };
      };
      toNumber = function(n) {
        return n;
      };
    }
  });

  // output/Data.Number/foreign.js
  var pow;
  var init_foreign105 = __esm({
    "output/Data.Number/foreign.js"() {
      pow = function(n) {
        return function(p2) {
          return Math.pow(n, p2);
        };
      };
    }
  });

  // output/Data.Number/index.js
  var init_Data44 = __esm({
    "output/Data.Number/index.js"() {
      init_foreign105();
      init_Data15();
      init_foreign105();
    }
  });

  // output/Data.Int/index.js
  var odd, fromNumber;
  var init_Data45 = __esm({
    "output/Data.Int/index.js"() {
      init_foreign104();
      init_Control2();
      init_Data();
      init_Data13();
      init_Data8();
      init_Data15();
      init_Data44();
      init_Data9();
      init_Data10();
      init_foreign104();
      odd = function(x) {
        return (x & 1) !== 0;
      };
      fromNumber = /* @__PURE__ */ function() {
        return fromNumberImpl(Just.create)(Nothing.value);
      }();
    }
  });

  // output/Data.List.NonEmpty/index.js
  var singleton5, cons2;
  var init_Data_List4 = __esm({
    "output/Data.List.NonEmpty/index.js"() {
      init_Control5();
      init_Control2();
      init_Data();
      init_Data8();
      init_Data26();
      init_Data2();
      init_Data4();
      init_Data43();
      init_Data_List();
      init_Data15();
      init_Data42();
      init_Data12();
      init_Data7();
      init_Data_Semigroup();
      init_Data_Semigroup2();
      init_Data29();
      init_Data22();
      init_Data31();
      init_Partial2();
      init_Data26();
      init_Data_List();
      init_Data_Semigroup();
      init_Data_Semigroup2();
      init_Data29();
      singleton5 = /* @__PURE__ */ function() {
        var $200 = singleton3(plusList);
        return function($201) {
          return NonEmptyList($200($201));
        };
      }();
      cons2 = function(y) {
        return function(v) {
          return new NonEmpty(y, new Cons(v.value0, v.value1));
        };
      };
    }
  });

  // output/Data.String.CodeUnits/foreign.js
  var fromCharArray;
  var init_foreign106 = __esm({
    "output/Data.String.CodeUnits/foreign.js"() {
      fromCharArray = function(a2) {
        return a2.join("");
      };
    }
  });

  // output/Data.String.Unsafe/foreign.js
  var init_foreign107 = __esm({
    "output/Data.String.Unsafe/foreign.js"() {
    }
  });

  // output/Data.String.Unsafe/index.js
  var init_Data_String2 = __esm({
    "output/Data.String.Unsafe/index.js"() {
      init_foreign107();
      init_foreign107();
    }
  });

  // output/Data.String.CodeUnits/index.js
  var init_Data_String3 = __esm({
    "output/Data.String.CodeUnits/index.js"() {
      init_foreign106();
      init_Data15();
      init_Data_String2();
      init_foreign106();
    }
  });

  // output/Foreign/index.js
  var init_Foreign = __esm({
    "output/Foreign/index.js"() {
      init_foreign103();
      init_Control4();
      init_Control_Monad_Error();
      init_Control_Monad_Except();
      init_Data();
      init_Data16();
      init_Data8();
      init_Data2();
      init_Data4();
      init_Data45();
      init_Data_List4();
      init_Data15();
      init_Data12();
      init_Data9();
      init_Data14();
      init_Data_String3();
      init_Unsafe();
      init_foreign103();
    }
  });

  // output/Foreign.Object/foreign.js
  function _lookup(no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  }
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys;
  var init_foreign108 = __esm({
    "output/Foreign.Object/foreign.js"() {
      keys = Object.keys || toArrayWithKey(function(k) {
        return function() {
          return k;
        };
      });
    }
  });

  // output/Foreign.Object/index.js
  var lookup2;
  var init_Foreign2 = __esm({
    "output/Foreign.Object/index.js"() {
      init_foreign108();
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control2();
      init_Control_Monad_ST();
      init_Data32();
      init_Data8();
      init_Data26();
      init_Data40();
      init_Data2();
      init_Data_Function();
      init_Data4();
      init_Data15();
      init_Data19();
      init_Data12();
      init_Data7();
      init_Data14();
      init_Data29();
      init_Data41();
      init_Data22();
      init_Data31();
      init_Foreign_Object();
      init_Unsafe();
      init_foreign108();
      lookup2 = /* @__PURE__ */ function() {
        return runFn4(_lookup)(Nothing.value)(Just.create);
      }();
    }
  });

  // output/Halogen.VDom.DOM.Prop/index.js
  var $runtime_lazy4, Created, Removed, Attribute, Property, Handler, Ref, unsafeGetProperty, setProperty, removeProperty, propToStrKey, propFromString, buildProp;
  var init_Halogen_VDom_DOM = __esm({
    "output/Halogen.VDom.DOM.Prop/index.js"() {
      init_Data4();
      init_Data15();
      init_Data36();
      init_Data22();
      init_Data3();
      init_Effect3();
      init_Foreign();
      init_Foreign2();
      init_Halogen_VDom();
      init_Halogen_VDom3();
      init_Unsafe();
      init_Web_Event();
      $runtime_lazy4 = function(name15, moduleName, init2) {
        var state3 = 0;
        var val;
        return function(lineNumber) {
          if (state3 === 2)
            return val;
          if (state3 === 1)
            throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
          state3 = 1;
          val = init2();
          state3 = 2;
          return val;
        };
      };
      Created = /* @__PURE__ */ function() {
        function Created2(value0) {
          this.value0 = value0;
        }
        ;
        Created2.create = function(value0) {
          return new Created2(value0);
        };
        return Created2;
      }();
      Removed = /* @__PURE__ */ function() {
        function Removed2(value0) {
          this.value0 = value0;
        }
        ;
        Removed2.create = function(value0) {
          return new Removed2(value0);
        };
        return Removed2;
      }();
      Attribute = /* @__PURE__ */ function() {
        function Attribute2(value0, value1, value22) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value22;
        }
        ;
        Attribute2.create = function(value0) {
          return function(value1) {
            return function(value22) {
              return new Attribute2(value0, value1, value22);
            };
          };
        };
        return Attribute2;
      }();
      Property = /* @__PURE__ */ function() {
        function Property2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        Property2.create = function(value0) {
          return function(value1) {
            return new Property2(value0, value1);
          };
        };
        return Property2;
      }();
      Handler = /* @__PURE__ */ function() {
        function Handler2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        Handler2.create = function(value0) {
          return function(value1) {
            return new Handler2(value0, value1);
          };
        };
        return Handler2;
      }();
      Ref = /* @__PURE__ */ function() {
        function Ref2(value0) {
          this.value0 = value0;
        }
        ;
        Ref2.create = function(value0) {
          return new Ref2(value0);
        };
        return Ref2;
      }();
      unsafeGetProperty = unsafeGetAny;
      setProperty = unsafeSetAny;
      removeProperty = function(key, el) {
        var v = hasAttribute(nullImpl, key, el);
        if (v) {
          return removeAttribute(nullImpl, key, el);
        }
        ;
        var v1 = typeOf(unsafeGetAny(key, el));
        if (v1 === "string") {
          return unsafeSetAny(key, "", el);
        }
        ;
        if (key === "rowSpan") {
          return unsafeSetAny(key, 1, el);
        }
        ;
        if (key === "colSpan") {
          return unsafeSetAny(key, 1, el);
        }
        ;
        return unsafeSetAny(key, jsUndefined, el);
      };
      propToStrKey = function(v) {
        if (v instanceof Attribute && v.value0 instanceof Just) {
          return "attr/" + (v.value0.value0 + (":" + v.value1));
        }
        ;
        if (v instanceof Attribute) {
          return "attr/:" + v.value1;
        }
        ;
        if (v instanceof Property) {
          return "prop/" + v.value0;
        }
        ;
        if (v instanceof Handler) {
          return "handler/" + v.value0;
        }
        ;
        if (v instanceof Ref) {
          return "ref";
        }
        ;
        throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 182, column 16 - line 187, column 16): " + [v.constructor.name]);
      };
      propFromString = unsafeCoerce2;
      buildProp = function(emit) {
        return function(el) {
          var removeProp = function(prevEvents) {
            return function(v, v1) {
              if (v1 instanceof Attribute) {
                return removeAttribute(toNullable(v1.value0), v1.value1, el);
              }
              ;
              if (v1 instanceof Property) {
                return removeProperty(v1.value0, el);
              }
              ;
              if (v1 instanceof Handler) {
                var handler3 = unsafeLookup(v1.value0, prevEvents);
                return removeEventListener2(v1.value0, fst(handler3), el);
              }
              ;
              if (v1 instanceof Ref) {
                return unit;
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 169, column 5 - line 179, column 18): " + [v1.constructor.name]);
            };
          };
          var mbEmit = function(v) {
            if (v instanceof Just) {
              return emit(v.value0)();
            }
            ;
            return unit;
          };
          var haltProp = function(state3) {
            var v = lookup2("ref")(state3.props);
            if (v instanceof Just && v.value0 instanceof Ref) {
              return mbEmit(v.value0.value0(new Removed(el)));
            }
            ;
            return unit;
          };
          var diffProp = function(prevEvents, events) {
            return function(v, v1, v11, v2) {
              if (v11 instanceof Attribute && v2 instanceof Attribute) {
                var $66 = v11.value2 === v2.value2;
                if ($66) {
                  return v2;
                }
                ;
                setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
                return v2;
              }
              ;
              if (v11 instanceof Property && v2 instanceof Property) {
                var v4 = refEq2(v11.value1, v2.value1);
                if (v4) {
                  return v2;
                }
                ;
                if (v2.value0 === "value") {
                  var elVal = unsafeGetProperty("value", el);
                  var $75 = refEq2(elVal, v2.value1);
                  if ($75) {
                    return v2;
                  }
                  ;
                  setProperty(v2.value0, v2.value1, el);
                  return v2;
                }
                ;
                setProperty(v2.value0, v2.value1, el);
                return v2;
              }
              ;
              if (v11 instanceof Handler && v2 instanceof Handler) {
                var handler3 = unsafeLookup(v2.value0, prevEvents);
                write(v2.value1)(snd(handler3))();
                pokeMutMap(v2.value0, handler3, events);
                return v2;
              }
              ;
              return v2;
            };
          };
          var applyProp = function(events) {
            return function(v, v1, v2) {
              if (v2 instanceof Attribute) {
                setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
                return v2;
              }
              ;
              if (v2 instanceof Property) {
                setProperty(v2.value0, v2.value1, el);
                return v2;
              }
              ;
              if (v2 instanceof Handler) {
                var v3 = unsafeGetAny(v2.value0, events);
                if (unsafeHasAny(v2.value0, events)) {
                  write(v2.value1)(snd(v3))();
                  return v2;
                }
                ;
                var ref2 = $$new(v2.value1)();
                var listener = eventListener(function(ev) {
                  return function __do2() {
                    var f$prime = read(ref2)();
                    return mbEmit(f$prime(ev));
                  };
                })();
                pokeMutMap(v2.value0, new Tuple(listener, ref2), events);
                addEventListener2(v2.value0, listener, el);
                return v2;
              }
              ;
              if (v2 instanceof Ref) {
                mbEmit(v2.value0(new Created(el)));
                return v2;
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 113, column 5 - line 135, column 15): " + [v2.constructor.name]);
            };
          };
          var $lazy_patchProp = $runtime_lazy4("patchProp", "Halogen.VDom.DOM.Prop", function() {
            return function(state3, ps2) {
              var events = newMutMap();
              var onThis = removeProp(state3.events);
              var onThese = diffProp(state3.events, events);
              var onThat = applyProp(events);
              var props = diffWithKeyAndIxE(state3.props, ps2, propToStrKey, onThese, onThis, onThat);
              var nextState = {
                events: unsafeFreeze2(events),
                props
              };
              return mkStep(new Step(unit, nextState, $lazy_patchProp(100), haltProp));
            };
          });
          var patchProp = $lazy_patchProp(87);
          var renderProp = function(ps1) {
            var events = newMutMap();
            var ps1$prime = strMapWithIxE(ps1, propToStrKey, applyProp(events));
            var state3 = {
              events: unsafeFreeze2(events),
              props: ps1$prime
            };
            return mkStep(new Step(unit, state3, patchProp, haltProp));
          };
          return renderProp;
        };
      };
    }
  });

  // output/Halogen.HTML.Core/index.js
  var HTML, toPropValue, text5, prop, isPropInputType, isPropInputAcceptType, handler, element;
  var init_Halogen_HTML = __esm({
    "output/Halogen.HTML.Core/index.js"() {
      init_DOM_HTML_Indexed3();
      init_DOM_HTML_Indexed4();
      init_DOM_HTML_Indexed5();
      init_DOM_HTML_Indexed6();
      init_DOM_HTML_Indexed7();
      init_DOM_HTML_Indexed();
      init_DOM_HTML_Indexed2();
      init_DOM_HTML_Indexed8();
      init_DOM_HTML_Indexed9();
      init_DOM_HTML_Indexed10();
      init_DOM_HTML_Indexed11();
      init_DOM_HTML_Indexed12();
      init_DOM_HTML_Indexed13();
      init_DOM_HTML_Indexed14();
      init_DOM_HTML_Indexed15();
      init_Data24();
      init_Data4();
      init_Data15();
      init_Data25();
      init_Halogen_Query();
      init_Halogen();
      init_Halogen_VDom_DOM();
      init_Halogen_VDom2();
      init_Web_HTML();
      init_Halogen();
      init_Halogen_VDom_DOM();
      init_Web_HTML();
      HTML = function(x) {
        return x;
      };
      toPropValue = function(dict) {
        return dict.toPropValue;
      };
      text5 = function($29) {
        return HTML(Text.create($29));
      };
      prop = function(dictIsProp) {
        var toPropValue1 = toPropValue(dictIsProp);
        return function(v) {
          var $31 = Property.create(v);
          return function($32) {
            return $31(toPropValue1($32));
          };
        };
      };
      isPropInputType = {
        toPropValue: function($45) {
          return propFromString(renderInputType($45));
        }
      };
      isPropInputAcceptType = {
        toPropValue: function($46) {
          return propFromString(renderInputAcceptType($46));
        }
      };
      handler = /* @__PURE__ */ function() {
        return Handler.create;
      }();
      element = function(ns) {
        return function(name15) {
          return function(props) {
            return function(children2) {
              return new Elem(ns, name15, props, children2);
            };
          };
        };
      };
    }
  });

  // output/Control.Applicative.Free/index.js
  var identity6, Pure, Lift, Ap, mkAp, liftFreeAp, goLeft, goApply, functorFreeAp, foldFreeAp, retractFreeAp, applyFreeAp, applicativeFreeAp, foldFreeAp1, hoistFreeAp;
  var init_Control_Applicative = __esm({
    "output/Control.Applicative.Free/index.js"() {
      init_Control4();
      init_Control3();
      init_Control2();
      init_Data27();
      init_Data16();
      init_Data_List4();
      init_Data_List();
      init_Data25();
      init_Data42();
      init_Data22();
      identity6 = /* @__PURE__ */ identity(categoryFn);
      Pure = /* @__PURE__ */ function() {
        function Pure2(value0) {
          this.value0 = value0;
        }
        ;
        Pure2.create = function(value0) {
          return new Pure2(value0);
        };
        return Pure2;
      }();
      Lift = /* @__PURE__ */ function() {
        function Lift3(value0) {
          this.value0 = value0;
        }
        ;
        Lift3.create = function(value0) {
          return new Lift3(value0);
        };
        return Lift3;
      }();
      Ap = /* @__PURE__ */ function() {
        function Ap2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        Ap2.create = function(value0) {
          return function(value1) {
            return new Ap2(value0, value1);
          };
        };
        return Ap2;
      }();
      mkAp = function(fba) {
        return function(fb) {
          return new Ap(fba, fb);
        };
      };
      liftFreeAp = /* @__PURE__ */ function() {
        return Lift.create;
      }();
      goLeft = function(dictApplicative) {
        var pure11 = pure(dictApplicative);
        return function(fStack) {
          return function(valStack) {
            return function(nat) {
              return function(func) {
                return function(count) {
                  if (func instanceof Pure) {
                    return new Tuple(new Cons({
                      func: pure11(func.value0),
                      count
                    }, fStack), valStack);
                  }
                  ;
                  if (func instanceof Lift) {
                    return new Tuple(new Cons({
                      func: nat(func.value0),
                      count
                    }, fStack), valStack);
                  }
                  ;
                  if (func instanceof Ap) {
                    return goLeft(dictApplicative)(fStack)(cons2(func.value1)(valStack))(nat)(func.value0)(count + 1 | 0);
                  }
                  ;
                  throw new Error("Failed pattern match at Control.Applicative.Free (line 102, column 41 - line 105, column 81): " + [func.constructor.name]);
                };
              };
            };
          };
        };
      };
      goApply = function(dictApplicative) {
        var apply2 = apply(dictApplicative.Apply0());
        return function(fStack) {
          return function(vals) {
            return function(gVal) {
              if (fStack instanceof Nil) {
                return new Left(gVal);
              }
              ;
              if (fStack instanceof Cons) {
                var gRes = apply2(fStack.value0.func)(gVal);
                var $31 = fStack.value0.count === 1;
                if ($31) {
                  if (fStack.value1 instanceof Nil) {
                    return new Left(gRes);
                  }
                  ;
                  return goApply(dictApplicative)(fStack.value1)(vals)(gRes);
                }
                ;
                if (vals instanceof Nil) {
                  return new Left(gRes);
                }
                ;
                if (vals instanceof Cons) {
                  return new Right(new Tuple(new Cons({
                    func: gRes,
                    count: fStack.value0.count - 1 | 0
                  }, fStack.value1), new NonEmpty(vals.value0, vals.value1)));
                }
                ;
                throw new Error("Failed pattern match at Control.Applicative.Free (line 83, column 11 - line 88, column 50): " + [vals.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 72, column 3 - line 88, column 50): " + [fStack.constructor.name]);
            };
          };
        };
      };
      functorFreeAp = {
        map: function(f) {
          return function(x) {
            return mkAp(new Pure(f))(x);
          };
        }
      };
      foldFreeAp = function(dictApplicative) {
        var goApply1 = goApply(dictApplicative);
        var pure11 = pure(dictApplicative);
        var goLeft1 = goLeft(dictApplicative);
        return function(nat) {
          return function(z) {
            var go2 = function($copy_v) {
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v) {
                if (v.value1.value0 instanceof Pure) {
                  var v1 = goApply1(v.value0)(v.value1.value1)(pure11(v.value1.value0.value0));
                  if (v1 instanceof Left) {
                    $tco_done = true;
                    return v1.value0;
                  }
                  ;
                  if (v1 instanceof Right) {
                    $copy_v = v1.value0;
                    return;
                  }
                  ;
                  throw new Error("Failed pattern match at Control.Applicative.Free (line 54, column 17 - line 56, column 24): " + [v1.constructor.name]);
                }
                ;
                if (v.value1.value0 instanceof Lift) {
                  var v1 = goApply1(v.value0)(v.value1.value1)(nat(v.value1.value0.value0));
                  if (v1 instanceof Left) {
                    $tco_done = true;
                    return v1.value0;
                  }
                  ;
                  if (v1 instanceof Right) {
                    $copy_v = v1.value0;
                    return;
                  }
                  ;
                  throw new Error("Failed pattern match at Control.Applicative.Free (line 57, column 17 - line 59, column 24): " + [v1.constructor.name]);
                }
                ;
                if (v.value1.value0 instanceof Ap) {
                  var nextVals = new NonEmpty(v.value1.value0.value1, v.value1.value1);
                  $copy_v = goLeft1(v.value0)(nextVals)(nat)(v.value1.value0.value0)(1);
                  return;
                }
                ;
                throw new Error("Failed pattern match at Control.Applicative.Free (line 53, column 5 - line 62, column 47): " + [v.value1.value0.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($copy_v);
              }
              ;
              return $tco_result;
            };
            return go2(new Tuple(Nil.value, singleton5(z)));
          };
        };
      };
      retractFreeAp = function(dictApplicative) {
        return foldFreeAp(dictApplicative)(identity6);
      };
      applyFreeAp = {
        apply: function(fba) {
          return function(fb) {
            return mkAp(fba)(fb);
          };
        },
        Functor0: function() {
          return functorFreeAp;
        }
      };
      applicativeFreeAp = /* @__PURE__ */ function() {
        return {
          pure: Pure.create,
          Apply0: function() {
            return applyFreeAp;
          }
        };
      }();
      foldFreeAp1 = /* @__PURE__ */ foldFreeAp(applicativeFreeAp);
      hoistFreeAp = function(f) {
        return foldFreeAp1(function($54) {
          return liftFreeAp(f($54));
        });
      };
    }
  });

  // output/Data.CatQueue/index.js
  var CatQueue, uncons2, snoc3, $$null2, empty5;
  var init_Data46 = __esm({
    "output/Data.CatQueue/index.js"() {
      init_Control4();
      init_Control3();
      init_Control6();
      init_Data8();
      init_Data26();
      init_Data2();
      init_Data4();
      init_Data43();
      init_Data_List();
      init_Data15();
      init_Data12();
      init_Data9();
      init_Data7();
      init_Data14();
      init_Data29();
      init_Data22();
      CatQueue = /* @__PURE__ */ function() {
        function CatQueue2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        CatQueue2.create = function(value0) {
          return function(value1) {
            return new CatQueue2(value0, value1);
          };
        };
        return CatQueue2;
      }();
      uncons2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v.value0 instanceof Nil) {
            $copy_v = new CatQueue(reverse2(v.value1), Nil.value);
            return;
          }
          ;
          if (v.value0 instanceof Cons) {
            $tco_done = true;
            return new Just(new Tuple(v.value0.value0, new CatQueue(v.value0.value1, v.value1)));
          }
          ;
          throw new Error("Failed pattern match at Data.CatQueue (line 82, column 1 - line 82, column 63): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      snoc3 = function(v) {
        return function(a2) {
          return new CatQueue(v.value0, new Cons(a2, v.value1));
        };
      };
      $$null2 = function(v) {
        if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
          return true;
        }
        ;
        return false;
      };
      empty5 = /* @__PURE__ */ function() {
        return new CatQueue(Nil.value, Nil.value);
      }();
    }
  });

  // output/Data.CatList/index.js
  var CatNil, CatCons, link, foldr4, uncons3, empty6, append2, semigroupCatList, snoc4;
  var init_Data47 = __esm({
    "output/Data.CatList/index.js"() {
      init_Control4();
      init_Control3();
      init_Control6();
      init_Data46();
      init_Data26();
      init_Data2();
      init_Data4();
      init_Data_List();
      init_Data15();
      init_Data19();
      init_Data7();
      init_Data10();
      init_Data14();
      init_Data29();
      init_Data22();
      CatNil = /* @__PURE__ */ function() {
        function CatNil2() {
        }
        ;
        CatNil2.value = new CatNil2();
        return CatNil2;
      }();
      CatCons = /* @__PURE__ */ function() {
        function CatCons2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        CatCons2.create = function(value0) {
          return function(value1) {
            return new CatCons2(value0, value1);
          };
        };
        return CatCons2;
      }();
      link = function(v) {
        return function(v1) {
          if (v instanceof CatNil) {
            return v1;
          }
          ;
          if (v1 instanceof CatNil) {
            return v;
          }
          ;
          if (v instanceof CatCons) {
            return new CatCons(v.value0, snoc3(v.value1)(v1));
          }
          ;
          throw new Error("Failed pattern match at Data.CatList (line 108, column 1 - line 108, column 54): " + [v.constructor.name, v1.constructor.name]);
        };
      };
      foldr4 = function(k) {
        return function(b2) {
          return function(q2) {
            var foldl2 = function($copy_v) {
              return function($copy_v1) {
                return function($copy_v2) {
                  var $tco_var_v = $copy_v;
                  var $tco_var_v1 = $copy_v1;
                  var $tco_done = false;
                  var $tco_result;
                  function $tco_loop(v, v1, v2) {
                    if (v2 instanceof Nil) {
                      $tco_done = true;
                      return v1;
                    }
                    ;
                    if (v2 instanceof Cons) {
                      $tco_var_v = v;
                      $tco_var_v1 = v(v1)(v2.value0);
                      $copy_v2 = v2.value1;
                      return;
                    }
                    ;
                    throw new Error("Failed pattern match at Data.CatList (line 124, column 3 - line 124, column 59): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
                  }
                  ;
                  while (!$tco_done) {
                    $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
                  }
                  ;
                  return $tco_result;
                };
              };
            };
            var go2 = function($copy_xs) {
              return function($copy_ys) {
                var $tco_var_xs = $copy_xs;
                var $tco_done1 = false;
                var $tco_result;
                function $tco_loop(xs, ys) {
                  var v = uncons2(xs);
                  if (v instanceof Nothing) {
                    $tco_done1 = true;
                    return foldl2(function(x) {
                      return function(i2) {
                        return i2(x);
                      };
                    })(b2)(ys);
                  }
                  ;
                  if (v instanceof Just) {
                    $tco_var_xs = v.value0.value1;
                    $copy_ys = new Cons(k(v.value0.value0), ys);
                    return;
                  }
                  ;
                  throw new Error("Failed pattern match at Data.CatList (line 120, column 14 - line 122, column 67): " + [v.constructor.name]);
                }
                ;
                while (!$tco_done1) {
                  $tco_result = $tco_loop($tco_var_xs, $copy_ys);
                }
                ;
                return $tco_result;
              };
            };
            return go2(q2)(Nil.value);
          };
        };
      };
      uncons3 = function(v) {
        if (v instanceof CatNil) {
          return Nothing.value;
        }
        ;
        if (v instanceof CatCons) {
          return new Just(new Tuple(v.value0, function() {
            var $66 = $$null2(v.value1);
            if ($66) {
              return CatNil.value;
            }
            ;
            return foldr4(link)(CatNil.value)(v.value1);
          }()));
        }
        ;
        throw new Error("Failed pattern match at Data.CatList (line 99, column 1 - line 99, column 61): " + [v.constructor.name]);
      };
      empty6 = /* @__PURE__ */ function() {
        return CatNil.value;
      }();
      append2 = link;
      semigroupCatList = {
        append: append2
      };
      snoc4 = function(cat) {
        return function(a2) {
          return append2(cat)(new CatCons(a2, empty5));
        };
      };
    }
  });

  // output/Control.Monad.Free/index.js
  var $runtime_lazy5, append3, Free, Return, Bind, toView, fromView, freeMonad, freeFunctor, freeBind, freeApplicative, $lazy_freeApply, pure4, liftF, foldFree;
  var init_Control_Monad = __esm({
    "output/Control.Monad.Free/index.js"() {
      init_Control4();
      init_Control3();
      init_Control5();
      init_Control2();
      init_Control6();
      init_Control_Monad_Rec();
      init_Data47();
      init_Data16();
      init_Data8();
      init_Data26();
      init_Data2();
      init_Data4();
      init_Data15();
      init_Data19();
      init_Data12();
      init_Data9();
      init_Data7();
      init_Data29();
      init_Unsafe();
      $runtime_lazy5 = function(name15, moduleName, init2) {
        var state3 = 0;
        var val;
        return function(lineNumber) {
          if (state3 === 2)
            return val;
          if (state3 === 1)
            throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
          state3 = 1;
          val = init2();
          state3 = 2;
          return val;
        };
      };
      append3 = /* @__PURE__ */ append(semigroupCatList);
      Free = /* @__PURE__ */ function() {
        function Free2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        Free2.create = function(value0) {
          return function(value1) {
            return new Free2(value0, value1);
          };
        };
        return Free2;
      }();
      Return = /* @__PURE__ */ function() {
        function Return2(value0) {
          this.value0 = value0;
        }
        ;
        Return2.create = function(value0) {
          return new Return2(value0);
        };
        return Return2;
      }();
      Bind = /* @__PURE__ */ function() {
        function Bind2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        Bind2.create = function(value0) {
          return function(value1) {
            return new Bind2(value0, value1);
          };
        };
        return Bind2;
      }();
      toView = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          var runExpF = function(v22) {
            return v22;
          };
          var concatF = function(v22) {
            return function(r) {
              return new Free(v22.value0, append3(v22.value1)(r));
            };
          };
          if (v.value0 instanceof Return) {
            var v2 = uncons3(v.value1);
            if (v2 instanceof Nothing) {
              $tco_done = true;
              return new Return(v.value0.value0);
            }
            ;
            if (v2 instanceof Just) {
              $copy_v = concatF(runExpF(v2.value0.value0)(v.value0.value0))(v2.value0.value1);
              return;
            }
            ;
            throw new Error("Failed pattern match at Control.Monad.Free (line 227, column 7 - line 231, column 64): " + [v2.constructor.name]);
          }
          ;
          if (v.value0 instanceof Bind) {
            $tco_done = true;
            return new Bind(v.value0.value0, function(a2) {
              return concatF(v.value0.value1(a2))(v.value1);
            });
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Free (line 225, column 3 - line 233, column 56): " + [v.value0.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      fromView = function(f) {
        return new Free(f, empty6);
      };
      freeMonad = {
        Applicative0: function() {
          return freeApplicative;
        },
        Bind1: function() {
          return freeBind;
        }
      };
      freeFunctor = {
        map: function(k) {
          return function(f) {
            return bindFlipped(freeBind)(function() {
              var $189 = pure(freeApplicative);
              return function($190) {
                return $189(k($190));
              };
            }())(f);
          };
        }
      };
      freeBind = {
        bind: function(v) {
          return function(k) {
            return new Free(v.value0, snoc4(v.value1)(k));
          };
        },
        Apply0: function() {
          return $lazy_freeApply(0);
        }
      };
      freeApplicative = {
        pure: function($191) {
          return fromView(Return.create($191));
        },
        Apply0: function() {
          return $lazy_freeApply(0);
        }
      };
      $lazy_freeApply = /* @__PURE__ */ $runtime_lazy5("freeApply", "Control.Monad.Free", function() {
        return {
          apply: ap(freeMonad),
          Functor0: function() {
            return freeFunctor;
          }
        };
      });
      pure4 = /* @__PURE__ */ pure(freeApplicative);
      liftF = function(f) {
        return fromView(new Bind(f, function($192) {
          return pure4($192);
        }));
      };
      foldFree = function(dictMonadRec) {
        var Monad0 = dictMonadRec.Monad0();
        var map110 = map(Monad0.Bind1().Apply0().Functor0());
        var pure13 = pure(Monad0.Applicative0());
        var tailRecM4 = tailRecM(dictMonadRec);
        return function(k) {
          var go2 = function(f) {
            var v = toView(f);
            if (v instanceof Return) {
              return map110(Done.create)(pure13(v.value0));
            }
            ;
            if (v instanceof Bind) {
              return map110(function($199) {
                return Loop.create(v.value1($199));
              })(k(v.value0));
            }
            ;
            throw new Error("Failed pattern match at Control.Monad.Free (line 158, column 10 - line 160, column 37): " + [v.constructor.name]);
          };
          return tailRecM4(go2);
        };
      };
    }
  });

  // output/Halogen.Query.ChildQuery/index.js
  var unChildQueryBox;
  var init_Halogen_Query2 = __esm({
    "output/Halogen.Query.ChildQuery/index.js"() {
      init_Unsafe();
      unChildQueryBox = unsafeCoerce2;
    }
  });

  // output/Unsafe.Reference/foreign.js
  function reallyUnsafeRefEq(a2) {
    return function(b2) {
      return a2 === b2;
    };
  }
  var init_foreign109 = __esm({
    "output/Unsafe.Reference/foreign.js"() {
    }
  });

  // output/Unsafe.Reference/index.js
  var unsafeRefEq;
  var init_Unsafe2 = __esm({
    "output/Unsafe.Reference/index.js"() {
      init_foreign109();
      init_Data8();
      init_foreign109();
      unsafeRefEq = reallyUnsafeRefEq;
    }
  });

  // output/Halogen.Subscription/index.js
  var $$void4, bind3, append4, traverse_2, traverse_1, unsubscribe, subscribe, notify, create3;
  var init_Halogen2 = __esm({
    "output/Halogen.Subscription/index.js"() {
      init_Control4();
      init_Control3();
      init_Control5();
      init_Data32();
      init_Data26();
      init_Data4();
      init_Data15();
      init_Data19();
      init_Data7();
      init_Data3();
      init_Effect();
      init_Effect3();
      init_Effect5();
      init_Safe();
      init_Unsafe2();
      $$void4 = /* @__PURE__ */ $$void(functorEffect);
      bind3 = /* @__PURE__ */ bind(bindEffect);
      append4 = /* @__PURE__ */ append(semigroupArray);
      traverse_2 = /* @__PURE__ */ traverse_(applicativeEffect);
      traverse_1 = /* @__PURE__ */ traverse_2(foldableArray);
      unsubscribe = function(v) {
        return v;
      };
      subscribe = function(v) {
        return function(k) {
          return v(function($76) {
            return $$void4(k($76));
          });
        };
      };
      notify = function(v) {
        return function(a2) {
          return v(a2);
        };
      };
      create3 = function __do() {
        var subscribers = $$new([])();
        return {
          emitter: function(k) {
            return function __do2() {
              modify_(function(v) {
                return append4(v)([k]);
              })(subscribers)();
              return modify_(deleteBy(unsafeRefEq)(k))(subscribers);
            };
          },
          listener: function(a2) {
            return bind3(read(subscribers))(traverse_1(function(k) {
              return k(a2);
            }));
          }
        };
      };
    }
  });

  // output/Halogen.Query.HalogenM/index.js
  var SubscriptionId, ForkId, State, Subscribe, Unsubscribe, Lift2, ChildQuery2, Raise, Par, Fork, Join, Kill, GetRef, HalogenM, ordSubscriptionId, ordForkId, monadHalogenM, monadStateHalogenM, monadEffectHalogenM, monadAffHalogenM, functorHalogenM, bindHalogenM, applicativeHalogenM;
  var init_Halogen_Query3 = __esm({
    "output/Halogen.Query.HalogenM/index.js"() {
      init_Control4();
      init_Control_Applicative();
      init_Control5();
      init_Control2();
      init_Control_Monad_Error();
      init_Control_Monad();
      init_Control_Monad_Reader();
      init_Control_Monad_Rec();
      init_Control_Monad_Writer();
      init_Data24();
      init_Data8();
      init_Data40();
      init_Data2();
      init_Data4();
      init_Data_Map();
      init_Data15();
      init_Data25();
      init_Data12();
      init_Data29();
      init_Data22();
      init_Data3();
      init_Effect_Aff();
      init_Effect4();
      init_Halogen_Data2();
      init_Halogen_Query2();
      init_Halogen2();
      SubscriptionId = function(x) {
        return x;
      };
      ForkId = function(x) {
        return x;
      };
      State = /* @__PURE__ */ function() {
        function State2(value0) {
          this.value0 = value0;
        }
        ;
        State2.create = function(value0) {
          return new State2(value0);
        };
        return State2;
      }();
      Subscribe = /* @__PURE__ */ function() {
        function Subscribe2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        Subscribe2.create = function(value0) {
          return function(value1) {
            return new Subscribe2(value0, value1);
          };
        };
        return Subscribe2;
      }();
      Unsubscribe = /* @__PURE__ */ function() {
        function Unsubscribe2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        Unsubscribe2.create = function(value0) {
          return function(value1) {
            return new Unsubscribe2(value0, value1);
          };
        };
        return Unsubscribe2;
      }();
      Lift2 = /* @__PURE__ */ function() {
        function Lift3(value0) {
          this.value0 = value0;
        }
        ;
        Lift3.create = function(value0) {
          return new Lift3(value0);
        };
        return Lift3;
      }();
      ChildQuery2 = /* @__PURE__ */ function() {
        function ChildQuery3(value0) {
          this.value0 = value0;
        }
        ;
        ChildQuery3.create = function(value0) {
          return new ChildQuery3(value0);
        };
        return ChildQuery3;
      }();
      Raise = /* @__PURE__ */ function() {
        function Raise2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        Raise2.create = function(value0) {
          return function(value1) {
            return new Raise2(value0, value1);
          };
        };
        return Raise2;
      }();
      Par = /* @__PURE__ */ function() {
        function Par2(value0) {
          this.value0 = value0;
        }
        ;
        Par2.create = function(value0) {
          return new Par2(value0);
        };
        return Par2;
      }();
      Fork = /* @__PURE__ */ function() {
        function Fork2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        Fork2.create = function(value0) {
          return function(value1) {
            return new Fork2(value0, value1);
          };
        };
        return Fork2;
      }();
      Join = /* @__PURE__ */ function() {
        function Join2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        Join2.create = function(value0) {
          return function(value1) {
            return new Join2(value0, value1);
          };
        };
        return Join2;
      }();
      Kill = /* @__PURE__ */ function() {
        function Kill2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        Kill2.create = function(value0) {
          return function(value1) {
            return new Kill2(value0, value1);
          };
        };
        return Kill2;
      }();
      GetRef = /* @__PURE__ */ function() {
        function GetRef2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        GetRef2.create = function(value0) {
          return function(value1) {
            return new GetRef2(value0, value1);
          };
        };
        return GetRef2;
      }();
      HalogenM = function(x) {
        return x;
      };
      ordSubscriptionId = ordInt;
      ordForkId = ordInt;
      monadHalogenM = freeMonad;
      monadStateHalogenM = {
        state: function($181) {
          return HalogenM(liftF(State.create($181)));
        },
        Monad0: function() {
          return monadHalogenM;
        }
      };
      monadEffectHalogenM = function(dictMonadEffect) {
        return {
          liftEffect: function() {
            var $186 = liftEffect(dictMonadEffect);
            return function($187) {
              return HalogenM(liftF(Lift2.create($186($187))));
            };
          }(),
          Monad0: function() {
            return monadHalogenM;
          }
        };
      };
      monadAffHalogenM = function(dictMonadAff) {
        var monadEffectHalogenM1 = monadEffectHalogenM(dictMonadAff.MonadEffect0());
        return {
          liftAff: function() {
            var $188 = liftAff(dictMonadAff);
            return function($189) {
              return HalogenM(liftF(Lift2.create($188($189))));
            };
          }(),
          MonadEffect0: function() {
            return monadEffectHalogenM1;
          }
        };
      };
      functorHalogenM = freeFunctor;
      bindHalogenM = freeBind;
      applicativeHalogenM = freeApplicative;
    }
  });

  // output/Halogen.Query.HalogenQ/index.js
  var Initialize, Finalize, Receive, Action2, Query;
  var init_Halogen_Query4 = __esm({
    "output/Halogen.Query.HalogenQ/index.js"() {
      init_Data39();
      init_Data4();
      Initialize = /* @__PURE__ */ function() {
        function Initialize2(value0) {
          this.value0 = value0;
        }
        ;
        Initialize2.create = function(value0) {
          return new Initialize2(value0);
        };
        return Initialize2;
      }();
      Finalize = /* @__PURE__ */ function() {
        function Finalize2(value0) {
          this.value0 = value0;
        }
        ;
        Finalize2.create = function(value0) {
          return new Finalize2(value0);
        };
        return Finalize2;
      }();
      Receive = /* @__PURE__ */ function() {
        function Receive2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        Receive2.create = function(value0) {
          return function(value1) {
            return new Receive2(value0, value1);
          };
        };
        return Receive2;
      }();
      Action2 = /* @__PURE__ */ function() {
        function Action3(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        Action3.create = function(value0) {
          return function(value1) {
            return new Action3(value0, value1);
          };
        };
        return Action3;
      }();
      Query = /* @__PURE__ */ function() {
        function Query2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        Query2.create = function(value0) {
          return function(value1) {
            return new Query2(value0, value1);
          };
        };
        return Query2;
      }();
    }
  });

  // output/Halogen.VDom.Thunk/index.js
  var $runtime_lazy6, unsafeEqThunk, runThunk, buildThunk;
  var init_Halogen_VDom5 = __esm({
    "output/Halogen.VDom.Thunk/index.js"() {
      init_Data_Function();
      init_Data4();
      init_Halogen_VDom4();
      init_Halogen_VDom();
      init_Halogen_VDom3();
      init_Unsafe();
      $runtime_lazy6 = function(name15, moduleName, init2) {
        var state3 = 0;
        var val;
        return function(lineNumber) {
          if (state3 === 2)
            return val;
          if (state3 === 1)
            throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
          state3 = 1;
          val = init2();
          state3 = 2;
          return val;
        };
      };
      unsafeEqThunk = function(v, v1) {
        return refEq2(v.value0, v1.value0) && (refEq2(v.value1, v1.value1) && v.value1(v.value3, v1.value3));
      };
      runThunk = function(v) {
        return v.value2(v.value3);
      };
      buildThunk = function(toVDom) {
        var haltThunk = function(state3) {
          return halt(state3.vdom);
        };
        var $lazy_patchThunk = $runtime_lazy6("patchThunk", "Halogen.VDom.Thunk", function() {
          return function(state3, t2) {
            var $48 = unsafeEqThunk(state3.thunk, t2);
            if ($48) {
              return mkStep(new Step(extract2(state3.vdom), state3, $lazy_patchThunk(112), haltThunk));
            }
            ;
            var vdom = step3(state3.vdom, toVDom(runThunk(t2)));
            return mkStep(new Step(extract2(vdom), {
              vdom,
              thunk: t2
            }, $lazy_patchThunk(115), haltThunk));
          };
        });
        var patchThunk = $lazy_patchThunk(108);
        var renderThunk = function(spec) {
          return function(t) {
            var vdom = buildVDom(spec)(toVDom(runThunk(t)));
            return mkStep(new Step(extract2(vdom), {
              thunk: t,
              vdom
            }, patchThunk, haltThunk));
          };
        };
        return renderThunk;
      };
    }
  });

  // output/Halogen.Component/index.js
  var voidLeft2, traverse_3, map11, pure5, ComponentSlot, ThunkSlot, unComponentSlot, unComponent, mkEval, mkComponent, defaultEval;
  var init_Halogen3 = __esm({
    "output/Halogen.Component/index.js"() {
      init_Control4();
      init_Data24();
      init_Data39();
      init_Data26();
      init_Data2();
      init_Data4();
      init_Data15();
      init_Data3();
      init_Halogen_Data2();
      init_Halogen_HTML();
      init_Halogen_Query3();
      init_Halogen_Query4();
      init_Halogen_VDom5();
      init_Unsafe();
      voidLeft2 = /* @__PURE__ */ voidLeft(functorHalogenM);
      traverse_3 = /* @__PURE__ */ traverse_(applicativeHalogenM)(foldableMaybe);
      map11 = /* @__PURE__ */ map(functorHalogenM);
      pure5 = /* @__PURE__ */ pure(applicativeHalogenM);
      ComponentSlot = /* @__PURE__ */ function() {
        function ComponentSlot2(value0) {
          this.value0 = value0;
        }
        ;
        ComponentSlot2.create = function(value0) {
          return new ComponentSlot2(value0);
        };
        return ComponentSlot2;
      }();
      ThunkSlot = /* @__PURE__ */ function() {
        function ThunkSlot2(value0) {
          this.value0 = value0;
        }
        ;
        ThunkSlot2.create = function(value0) {
          return new ThunkSlot2(value0);
        };
        return ThunkSlot2;
      }();
      unComponentSlot = unsafeCoerce2;
      unComponent = unsafeCoerce2;
      mkEval = function(args) {
        return function(v) {
          if (v instanceof Initialize) {
            return voidLeft2(traverse_3(args.handleAction)(args.initialize))(v.value0);
          }
          ;
          if (v instanceof Finalize) {
            return voidLeft2(traverse_3(args.handleAction)(args.finalize))(v.value0);
          }
          ;
          if (v instanceof Receive) {
            return voidLeft2(traverse_3(args.handleAction)(args.receive(v.value0)))(v.value1);
          }
          ;
          if (v instanceof Action2) {
            return voidLeft2(args.handleAction(v.value0))(v.value1);
          }
          ;
          if (v instanceof Query) {
            return unCoyoneda(function(g) {
              var $45 = map11(maybe(v.value1(unit))(g));
              return function($46) {
                return $45(args.handleQuery($46));
              };
            })(v.value0);
          }
          ;
          throw new Error("Failed pattern match at Halogen.Component (line 182, column 15 - line 192, column 71): " + [v.constructor.name]);
        };
      };
      mkComponent = unsafeCoerce2;
      defaultEval = /* @__PURE__ */ function() {
        return {
          handleAction: $$const(pure5(unit)),
          handleQuery: $$const(pure5(Nothing.value)),
          receive: $$const(Nothing.value),
          initialize: Nothing.value,
          finalize: Nothing.value
        };
      }();
    }
  });

  // output/Halogen.HTML.Elements/index.js
  var element2, form, form_, h1, h1_, input, p, p_, div2, div_;
  var init_Halogen_HTML2 = __esm({
    "output/Halogen.HTML.Elements/index.js"() {
      init_Control4();
      init_Data15();
      init_Halogen_HTML();
      init_Halogen_VDom2();
      element2 = /* @__PURE__ */ function() {
        return element(Nothing.value);
      }();
      form = /* @__PURE__ */ element2("form");
      form_ = /* @__PURE__ */ form([]);
      h1 = /* @__PURE__ */ element2("h1");
      h1_ = /* @__PURE__ */ h1([]);
      input = function(props) {
        return element2("input")(props)([]);
      };
      p = /* @__PURE__ */ element2("p");
      p_ = /* @__PURE__ */ p([]);
      div2 = /* @__PURE__ */ element2("div");
      div_ = /* @__PURE__ */ div2([]);
    }
  });

  // output/Control.Monad.Except/index.js
  var init_Control_Monad2 = __esm({
    "output/Control.Monad.Except/index.js"() {
      init_Control_Monad_Error();
      init_Control_Monad_Except();
      init_Data20();
      init_Data25();
      init_Control_Monad_Error();
      init_Control_Monad_Except();
    }
  });

  // output/Foreign.Index/foreign.js
  var init_foreign110 = __esm({
    "output/Foreign.Index/foreign.js"() {
    }
  });

  // output/Foreign.Index/index.js
  var init_Foreign3 = __esm({
    "output/Foreign.Index/index.js"() {
      init_foreign110();
      init_Control4();
      init_Control5();
      init_Control_Monad_Except();
      init_Data2();
      init_Foreign();
    }
  });

  // output/Web.Clipboard.ClipboardEvent.EventTypes/index.js
  var init_Web_Clipboard_ClipboardEvent = __esm({
    "output/Web.Clipboard.ClipboardEvent.EventTypes/index.js"() {
    }
  });

  // output/Web.Event.Event/foreign.js
  var init_foreign111 = __esm({
    "output/Web.Event.Event/foreign.js"() {
    }
  });

  // output/Web.Event.EventPhase/index.js
  var init_Web_Event2 = __esm({
    "output/Web.Event.EventPhase/index.js"() {
      init_Data37();
      init_Data15();
      init_Data9();
    }
  });

  // output/Web.Event.Event/index.js
  var init_Web_Event3 = __esm({
    "output/Web.Event.Event/index.js"() {
      init_foreign111();
      init_Data37();
      init_Data8();
      init_Data15();
      init_Data36();
      init_Data12();
      init_Web_Event2();
      init_foreign111();
    }
  });

  // output/Web.File.FileList/foreign.js
  var init_foreign112 = __esm({
    "output/Web.File.FileList/foreign.js"() {
    }
  });

  // output/Web.File.FileList/index.js
  var init_Web_File = __esm({
    "output/Web.File.FileList/index.js"() {
      init_foreign112();
      init_Data2();
      init_Data4();
      init_Data15();
      init_Data36();
      init_Data22();
      init_Data31();
      init_foreign112();
    }
  });

  // output/Web.HTML.Event.DragEvent.EventTypes/index.js
  var init_Web_HTML_Event_DragEvent = __esm({
    "output/Web.HTML.Event.DragEvent.EventTypes/index.js"() {
    }
  });

  // output/Web.UIEvent.FocusEvent.EventTypes/index.js
  var init_Web_UIEvent_FocusEvent = __esm({
    "output/Web.UIEvent.FocusEvent.EventTypes/index.js"() {
    }
  });

  // output/Web.UIEvent.KeyboardEvent.EventTypes/index.js
  var init_Web_UIEvent_KeyboardEvent = __esm({
    "output/Web.UIEvent.KeyboardEvent.EventTypes/index.js"() {
    }
  });

  // output/Web.UIEvent.MouseEvent.EventTypes/index.js
  var init_Web_UIEvent_MouseEvent = __esm({
    "output/Web.UIEvent.MouseEvent.EventTypes/index.js"() {
    }
  });

  // output/Web.UIEvent.WheelEvent.EventTypes/index.js
  var init_Web_UIEvent_WheelEvent = __esm({
    "output/Web.UIEvent.WheelEvent.EventTypes/index.js"() {
    }
  });

  // output/Halogen.HTML.Events/index.js
  var handler2, onChange;
  var init_Halogen_HTML3 = __esm({
    "output/Halogen.HTML.Events/index.js"() {
      init_Control5();
      init_Control_Monad2();
      init_Control_Monad_Except();
      init_Data16();
      init_Data2();
      init_Data4();
      init_Data20();
      init_Data15();
      init_Data31();
      init_Effect5();
      init_Foreign();
      init_Foreign3();
      init_Halogen_HTML();
      init_Halogen_Query();
      init_Unsafe();
      init_Web_Clipboard_ClipboardEvent();
      init_Web_Event3();
      init_Web_File();
      init_Web_HTML_Event_DragEvent();
      init_Web_HTML_Event();
      init_Web_HTML25();
      init_Web_UIEvent_FocusEvent();
      init_Web_UIEvent_KeyboardEvent();
      init_Web_UIEvent_MouseEvent();
      init_Web_UIEvent_WheelEvent();
      handler2 = function(et) {
        return function(f) {
          return handler(et)(function(ev) {
            return new Just(new Action(f(ev)));
          });
        };
      };
      onChange = /* @__PURE__ */ handler2(change);
    }
  });

  // output/Halogen.HTML.Properties/index.js
  var prop2, type_18, accept2;
  var init_Halogen_HTML4 = __esm({
    "output/Halogen.HTML.Properties/index.js"() {
      init_Control4();
      init_DOM_HTML_Indexed3();
      init_DOM_HTML_Indexed4();
      init_DOM_HTML_Indexed7();
      init_DOM_HTML_Indexed();
      init_DOM_HTML_Indexed2();
      init_DOM_HTML_Indexed9();
      init_DOM_HTML_Indexed10();
      init_DOM_HTML_Indexed11();
      init_DOM_HTML_Indexed12();
      init_DOM_HTML_Indexed13();
      init_DOM_HTML_Indexed14();
      init_Data4();
      init_Data21();
      init_Data15();
      init_Data25();
      init_Data_String();
      init_Halogen_HTML();
      init_Halogen_Query();
      init_Halogen_VDom_DOM();
      init_Unsafe();
      init_DOM_HTML_Indexed3();
      init_DOM_HTML_Indexed4();
      init_DOM_HTML_Indexed7();
      init_DOM_HTML_Indexed();
      init_DOM_HTML_Indexed2();
      init_DOM_HTML_Indexed9();
      init_DOM_HTML_Indexed10();
      init_DOM_HTML_Indexed11();
      init_DOM_HTML_Indexed12();
      init_DOM_HTML_Indexed13();
      init_DOM_HTML_Indexed14();
      prop2 = function(dictIsProp) {
        return prop(dictIsProp);
      };
      type_18 = function(dictIsProp) {
        return prop2(dictIsProp)("type");
      };
      accept2 = /* @__PURE__ */ prop2(isPropInputAcceptType)("accept");
    }
  });

  // output/Control.Monad.Fork.Class/index.js
  var monadForkAff, fork;
  var init_Control_Monad_Fork = __esm({
    "output/Control.Monad.Fork.Class/index.js"() {
      init_Control_Monad_Reader2();
      init_Control_Monad_Trans();
      init_Effect6();
      monadForkAff = {
        suspend: suspendAff,
        fork: forkAff,
        join: joinFiber,
        Monad0: function() {
          return monadAff;
        },
        Functor1: function() {
          return functorFiber;
        }
      };
      fork = function(dict) {
        return dict.fork;
      };
    }
  });

  // output/Effect.Console/foreign.js
  var warn;
  var init_foreign113 = __esm({
    "output/Effect.Console/foreign.js"() {
      warn = function(s) {
        return function() {
          console.warn(s);
        };
      };
    }
  });

  // output/Effect.Console/index.js
  var init_Effect8 = __esm({
    "output/Effect.Console/index.js"() {
      init_foreign113();
      init_Data14();
      init_foreign113();
    }
  });

  // output/Halogen.HTML/index.js
  var init_Halogen4 = __esm({
    "output/Halogen.HTML/index.js"() {
      init_Data2();
      init_Data4();
      init_Data15();
      init_Halogen3();
      init_Halogen_HTML();
      init_Halogen_HTML2();
      init_Halogen_HTML4();
      init_Halogen_VDom5();
      init_Unsafe();
      init_Halogen_HTML();
      init_Halogen_HTML2();
      init_Halogen_HTML4();
    }
  });

  // output/Halogen.Query/index.js
  var init_Halogen5 = __esm({
    "output/Halogen.Query/index.js"() {
      init_Control5();
      init_Control2();
      init_Control_Monad_State();
      init_Control_Monad_Trans();
      init_Data4();
      init_Data15();
      init_Data3();
      init_Effect_Aff();
      init_Effect4();
      init_Halogen_Query3();
      init_Halogen_Query4();
      init_Halogen_Query();
      init_Web_HTML15();
      init_Control_Monad_State();
      init_Control_Monad_Trans();
      init_Effect_Aff();
      init_Effect4();
      init_Halogen_Query3();
      init_Halogen_Query4();
      init_Halogen_Query();
    }
  });

  // output/Halogen/index.js
  var init_Halogen6 = __esm({
    "output/Halogen/index.js"() {
      init_Data35();
      init_Halogen3();
      init_Halogen_Data2();
      init_Halogen4();
      init_Halogen_HTML();
      init_Halogen5();
      init_Data35();
      init_Halogen3();
      init_Halogen_HTML();
      init_Halogen5();
    }
  });

  // output/Halogen.Aff.Driver.State/index.js
  var unRenderStateX, unDriverStateX, renderStateX_, mkRenderStateX, renderStateX, mkDriverStateXRef, mapDriverState, initDriverState;
  var init_Halogen_Aff_Driver = __esm({
    "output/Halogen.Aff.Driver.State/index.js"() {
      init_Data26();
      init_Data_List();
      init_Data_Map();
      init_Data15();
      init_Effect3();
      init_Halogen_Data2();
      init_Unsafe();
      unRenderStateX = unsafeCoerce2;
      unDriverStateX = unsafeCoerce2;
      renderStateX_ = function(dictApplicative) {
        var traverse_7 = traverse_(dictApplicative)(foldableMaybe);
        return function(f) {
          return unDriverStateX(function(st) {
            return traverse_7(f)(st.rendering);
          });
        };
      };
      mkRenderStateX = unsafeCoerce2;
      renderStateX = function(dictFunctor) {
        return function(f) {
          return unDriverStateX(function(st) {
            return mkRenderStateX(f(st.rendering));
          });
        };
      };
      mkDriverStateXRef = unsafeCoerce2;
      mapDriverState = function(f) {
        return function(v) {
          return f(v);
        };
      };
      initDriverState = function(component2) {
        return function(input3) {
          return function(handler3) {
            return function(lchs) {
              return function __do2() {
                var selfRef = $$new({})();
                var childrenIn = $$new(empty3)();
                var childrenOut = $$new(empty3)();
                var handlerRef = $$new(handler3)();
                var pendingQueries = $$new(new Just(Nil.value))();
                var pendingOuts = $$new(new Just(Nil.value))();
                var pendingHandlers = $$new(Nothing.value)();
                var fresh2 = $$new(1)();
                var subscriptions = $$new(new Just(empty2))();
                var forks = $$new(empty2)();
                var ds = {
                  component: component2,
                  state: component2.initialState(input3),
                  refs: empty2,
                  children: empty3,
                  childrenIn,
                  childrenOut,
                  selfRef,
                  handlerRef,
                  pendingQueries,
                  pendingOuts,
                  pendingHandlers,
                  rendering: Nothing.value,
                  fresh: fresh2,
                  subscriptions,
                  forks,
                  lifecycleHandlers: lchs
                };
                write(ds)(selfRef)();
                return mkDriverStateXRef(selfRef);
              };
            };
          };
        };
      };
    }
  });

  // output/Halogen.Aff.Driver.Eval/index.js
  var traverse_4, bindFlipped5, lookup4, bind12, liftEffect4, discard3, discard1, traverse_12, traverse_22, fork3, parSequence_2, pure6, map14, parallel2, map15, sequential2, map22, insert3, retractFreeAp2, $$delete2, unlessM2, insert1, traverse_32, lookup1, lookup22, foldFree2, alter2, unsubscribe3, queueOrRun, handleLifecycle, handleAff, fresh, evalQ, evalM, evalF;
  var init_Halogen_Aff_Driver2 = __esm({
    "output/Halogen.Aff.Driver.Eval/index.js"() {
      init_Control4();
      init_Control_Applicative();
      init_Control5();
      init_Control6();
      init_Control_Monad_Fork();
      init_Control_Monad();
      init_Control12();
      init_Control_Parallel();
      init_Data();
      init_Data39();
      init_Data16();
      init_Data26();
      init_Data2();
      init_Data4();
      init_Data_List();
      init_Data_Map();
      init_Data15();
      init_Data12();
      init_Data3();
      init_Effect();
      init_Effect6();
      init_Effect4();
      init_Effect2();
      init_Effect3();
      init_Halogen_Aff_Driver();
      init_Halogen_Query2();
      init_Halogen_Query3();
      init_Halogen_Query4();
      init_Halogen_Query();
      init_Halogen2();
      init_Unsafe2();
      traverse_4 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
      bindFlipped5 = /* @__PURE__ */ bindFlipped(bindMaybe);
      lookup4 = /* @__PURE__ */ lookup(ordSubscriptionId);
      bind12 = /* @__PURE__ */ bind(bindAff);
      liftEffect4 = /* @__PURE__ */ liftEffect(monadEffectAff);
      discard3 = /* @__PURE__ */ discard(discardUnit);
      discard1 = /* @__PURE__ */ discard3(bindAff);
      traverse_12 = /* @__PURE__ */ traverse_(applicativeAff);
      traverse_22 = /* @__PURE__ */ traverse_12(foldableList);
      fork3 = /* @__PURE__ */ fork(monadForkAff);
      parSequence_2 = /* @__PURE__ */ parSequence_(parallelAff)(foldableList);
      pure6 = /* @__PURE__ */ pure(applicativeAff);
      map14 = /* @__PURE__ */ map(functorCoyoneda);
      parallel2 = /* @__PURE__ */ parallel(parallelAff);
      map15 = /* @__PURE__ */ map(functorAff);
      sequential2 = /* @__PURE__ */ sequential(parallelAff);
      map22 = /* @__PURE__ */ map(functorMaybe);
      insert3 = /* @__PURE__ */ insert(ordSubscriptionId);
      retractFreeAp2 = /* @__PURE__ */ retractFreeAp(applicativeParAff);
      $$delete2 = /* @__PURE__ */ $$delete(ordForkId);
      unlessM2 = /* @__PURE__ */ unlessM(monadEffect);
      insert1 = /* @__PURE__ */ insert(ordForkId);
      traverse_32 = /* @__PURE__ */ traverse_12(foldableMaybe);
      lookup1 = /* @__PURE__ */ lookup(ordForkId);
      lookup22 = /* @__PURE__ */ lookup(ordString);
      foldFree2 = /* @__PURE__ */ foldFree(monadRecAff);
      alter2 = /* @__PURE__ */ alter(ordString);
      unsubscribe3 = function(sid) {
        return function(ref2) {
          return function __do2() {
            var v = read(ref2)();
            var subs = read(v.subscriptions)();
            return traverse_4(unsubscribe)(bindFlipped5(lookup4(sid))(subs))();
          };
        };
      };
      queueOrRun = function(ref2) {
        return function(au) {
          return bind12(liftEffect4(read(ref2)))(function(v) {
            if (v instanceof Nothing) {
              return au;
            }
            ;
            if (v instanceof Just) {
              return liftEffect4(write(new Just(new Cons(au, v.value0)))(ref2));
            }
            ;
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 188, column 33 - line 190, column 57): " + [v.constructor.name]);
          });
        };
      };
      handleLifecycle = function(lchs) {
        return function(f) {
          return discard1(liftEffect4(write({
            initializers: Nil.value,
            finalizers: Nil.value
          })(lchs)))(function() {
            return bind12(liftEffect4(f))(function(result) {
              return bind12(liftEffect4(read(lchs)))(function(v) {
                return discard1(traverse_22(fork3)(v.finalizers))(function() {
                  return discard1(parSequence_2(v.initializers))(function() {
                    return pure6(result);
                  });
                });
              });
            });
          });
        };
      };
      handleAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure(applicativeEffect)(unit))));
      fresh = function(f) {
        return function(ref2) {
          return bind12(liftEffect4(read(ref2)))(function(v) {
            return liftEffect4(modify$prime(function(i2) {
              return {
                state: i2 + 1 | 0,
                value: f(i2)
              };
            })(v.fresh));
          });
        };
      };
      evalQ = function(render2) {
        return function(ref2) {
          return function(q2) {
            return bind12(liftEffect4(read(ref2)))(function(v) {
              return evalM(render2)(ref2)(v["component"]["eval"](new Query(map14(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
            });
          };
        };
      };
      evalM = function(render2) {
        return function(initRef) {
          return function(v) {
            var evalChildQuery = function(ref2) {
              return function(cqb) {
                return bind12(liftEffect4(read(ref2)))(function(v1) {
                  return unChildQueryBox(function(v2) {
                    var evalChild = function(v3) {
                      return parallel2(bind12(liftEffect4(read(v3)))(function(dsx) {
                        return unDriverStateX(function(ds) {
                          return evalQ(render2)(ds.selfRef)(v2.value1);
                        })(dsx);
                      }));
                    };
                    return map15(v2.value2)(sequential2(v2.value0(applicativeParAff)(evalChild)(v1.children)));
                  })(cqb);
                });
              };
            };
            var go2 = function(ref2) {
              return function(v1) {
                if (v1 instanceof State) {
                  return bind12(liftEffect4(read(ref2)))(function(v2) {
                    var v3 = v1.value0(v2.state);
                    if (unsafeRefEq(v2.state)(v3.value1)) {
                      return pure6(v3.value0);
                    }
                    ;
                    if (otherwise) {
                      return discard1(liftEffect4(write({
                        component: v2.component,
                        refs: v2.refs,
                        children: v2.children,
                        childrenIn: v2.childrenIn,
                        childrenOut: v2.childrenOut,
                        selfRef: v2.selfRef,
                        handlerRef: v2.handlerRef,
                        pendingQueries: v2.pendingQueries,
                        pendingOuts: v2.pendingOuts,
                        pendingHandlers: v2.pendingHandlers,
                        rendering: v2.rendering,
                        fresh: v2.fresh,
                        subscriptions: v2.subscriptions,
                        forks: v2.forks,
                        lifecycleHandlers: v2.lifecycleHandlers,
                        state: v3.value1
                      })(ref2)))(function() {
                        return discard1(handleLifecycle(v2.lifecycleHandlers)(render2(v2.lifecycleHandlers)(ref2)))(function() {
                          return pure6(v3.value0);
                        });
                      });
                    }
                    ;
                    throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 86, column 7 - line 92, column 21): " + [v3.constructor.name]);
                  });
                }
                ;
                if (v1 instanceof Subscribe) {
                  return bind12(fresh(SubscriptionId)(ref2))(function(sid) {
                    return bind12(liftEffect4(subscribe(v1.value0(sid))(function(act) {
                      return handleAff(evalF(render2)(ref2)(new Action(act)));
                    })))(function(finalize) {
                      return bind12(liftEffect4(read(ref2)))(function(v2) {
                        return discard1(liftEffect4(modify_(map22(insert3(sid)(finalize)))(v2.subscriptions)))(function() {
                          return pure6(v1.value1(sid));
                        });
                      });
                    });
                  });
                }
                ;
                if (v1 instanceof Unsubscribe) {
                  return discard1(liftEffect4(unsubscribe3(v1.value0)(ref2)))(function() {
                    return pure6(v1.value1);
                  });
                }
                ;
                if (v1 instanceof Lift2) {
                  return v1.value0;
                }
                ;
                if (v1 instanceof ChildQuery2) {
                  return evalChildQuery(ref2)(v1.value0);
                }
                ;
                if (v1 instanceof Raise) {
                  return bind12(liftEffect4(read(ref2)))(function(v2) {
                    return bind12(liftEffect4(read(v2.handlerRef)))(function(handler3) {
                      return discard1(queueOrRun(v2.pendingOuts)(handler3(v1.value0)))(function() {
                        return pure6(v1.value1);
                      });
                    });
                  });
                }
                ;
                if (v1 instanceof Par) {
                  return sequential2(retractFreeAp2(hoistFreeAp(function() {
                    var $118 = evalM(render2)(ref2);
                    return function($119) {
                      return parallel2($118($119));
                    };
                  }())(v1.value0)));
                }
                ;
                if (v1 instanceof Fork) {
                  return bind12(fresh(ForkId)(ref2))(function(fid) {
                    return bind12(liftEffect4(read(ref2)))(function(v2) {
                      return bind12(liftEffect4($$new(false)))(function(doneRef) {
                        return bind12(fork3($$finally(liftEffect4(function __do2() {
                          modify_($$delete2(fid))(v2.forks)();
                          return write(true)(doneRef)();
                        }))(evalM(render2)(ref2)(v1.value0))))(function(fiber) {
                          return discard1(liftEffect4(unlessM2(read(doneRef))(modify_(insert1(fid)(fiber))(v2.forks))))(function() {
                            return pure6(v1.value1(fid));
                          });
                        });
                      });
                    });
                  });
                }
                ;
                if (v1 instanceof Join) {
                  return bind12(liftEffect4(read(ref2)))(function(v2) {
                    return bind12(liftEffect4(read(v2.forks)))(function(forkMap) {
                      return discard1(traverse_32(joinFiber)(lookup1(v1.value0)(forkMap)))(function() {
                        return pure6(v1.value1);
                      });
                    });
                  });
                }
                ;
                if (v1 instanceof Kill) {
                  return bind12(liftEffect4(read(ref2)))(function(v2) {
                    return bind12(liftEffect4(read(v2.forks)))(function(forkMap) {
                      return discard1(traverse_32(killFiber(error("Cancelled")))(lookup1(v1.value0)(forkMap)))(function() {
                        return pure6(v1.value1);
                      });
                    });
                  });
                }
                ;
                if (v1 instanceof GetRef) {
                  return bind12(liftEffect4(read(ref2)))(function(v2) {
                    return pure6(v1.value1(lookup22(v1.value0)(v2.refs)));
                  });
                }
                ;
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 83, column 12 - line 139, column 33): " + [v1.constructor.name]);
              };
            };
            return foldFree2(go2(initRef))(v);
          };
        };
      };
      evalF = function(render2) {
        return function(ref2) {
          return function(v) {
            if (v instanceof RefUpdate) {
              return liftEffect4(flip(modify_)(ref2)(mapDriverState(function(st) {
                return {
                  component: st.component,
                  state: st.state,
                  children: st.children,
                  childrenIn: st.childrenIn,
                  childrenOut: st.childrenOut,
                  selfRef: st.selfRef,
                  handlerRef: st.handlerRef,
                  pendingQueries: st.pendingQueries,
                  pendingOuts: st.pendingOuts,
                  pendingHandlers: st.pendingHandlers,
                  rendering: st.rendering,
                  fresh: st.fresh,
                  subscriptions: st.subscriptions,
                  forks: st.forks,
                  lifecycleHandlers: st.lifecycleHandlers,
                  refs: alter2($$const(v.value1))(v.value0)(st.refs)
                };
              })));
            }
            ;
            if (v instanceof Action) {
              return bind12(liftEffect4(read(ref2)))(function(v1) {
                return evalM(render2)(ref2)(v1["component"]["eval"](new Action2(v.value0, unit)));
              });
            }
            ;
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 52, column 20 - line 58, column 62): " + [v.constructor.name]);
          };
        };
      };
    }
  });

  // output/Halogen.Aff.Driver/index.js
  var bind4, discard4, for_2, traverse_5, fork4, bindFlipped6, traverse_13, traverse_23, traverse_33, discard22, parSequence_3, liftEffect5, pure7, map16, pure12, when2, renderStateX2, $$void5, foreachSlot2, renderStateX_2, tailRecM3, voidLeft3, bind13, liftEffect1, newLifecycleHandlers, handlePending, cleanupSubscriptionsAndForks, runUI;
  var init_Halogen_Aff2 = __esm({
    "output/Halogen.Aff.Driver/index.js"() {
      init_Control4();
      init_Control5();
      init_Control_Monad_Fork();
      init_Control_Monad_Rec();
      init_Control12();
      init_Data26();
      init_Data2();
      init_Data4();
      init_Data43();
      init_Data_List();
      init_Data_Map();
      init_Data15();
      init_Data3();
      init_Effect();
      init_Effect6();
      init_Effect4();
      init_Effect8();
      init_Effect2();
      init_Effect3();
      init_Halogen6();
      init_Halogen_Aff_Driver2();
      init_Halogen_Aff_Driver();
      init_Halogen3();
      init_Halogen_Data2();
      init_Halogen_Query4();
      init_Halogen_Query();
      init_Halogen2();
      bind4 = /* @__PURE__ */ bind(bindEffect);
      discard4 = /* @__PURE__ */ discard(discardUnit);
      for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
      traverse_5 = /* @__PURE__ */ traverse_(applicativeAff)(foldableList);
      fork4 = /* @__PURE__ */ fork(monadForkAff);
      bindFlipped6 = /* @__PURE__ */ bindFlipped(bindEffect);
      traverse_13 = /* @__PURE__ */ traverse_(applicativeEffect);
      traverse_23 = /* @__PURE__ */ traverse_13(foldableMaybe);
      traverse_33 = /* @__PURE__ */ traverse_13(foldableMap);
      discard22 = /* @__PURE__ */ discard4(bindAff);
      parSequence_3 = /* @__PURE__ */ parSequence_(parallelAff)(foldableList);
      liftEffect5 = /* @__PURE__ */ liftEffect(monadEffectAff);
      pure7 = /* @__PURE__ */ pure(applicativeEffect);
      map16 = /* @__PURE__ */ map(functorEffect);
      pure12 = /* @__PURE__ */ pure(applicativeAff);
      when2 = /* @__PURE__ */ when(applicativeEffect);
      renderStateX2 = /* @__PURE__ */ renderStateX(functorEffect);
      $$void5 = /* @__PURE__ */ $$void(functorAff);
      foreachSlot2 = /* @__PURE__ */ foreachSlot(applicativeEffect);
      renderStateX_2 = /* @__PURE__ */ renderStateX_(applicativeEffect);
      tailRecM3 = /* @__PURE__ */ tailRecM(monadRecEffect);
      voidLeft3 = /* @__PURE__ */ voidLeft(functorEffect);
      bind13 = /* @__PURE__ */ bind(bindAff);
      liftEffect1 = /* @__PURE__ */ liftEffect(monadEffectEffect);
      newLifecycleHandlers = /* @__PURE__ */ function() {
        return $$new({
          initializers: Nil.value,
          finalizers: Nil.value
        });
      }();
      handlePending = function(ref2) {
        return function __do2() {
          var queue = read(ref2)();
          write(Nothing.value)(ref2)();
          return for_2(queue)(function() {
            var $58 = traverse_5(fork4);
            return function($59) {
              return handleAff($58(reverse2($59)));
            };
          }())();
        };
      };
      cleanupSubscriptionsAndForks = function(v) {
        return function __do2() {
          bindFlipped6(traverse_23(traverse_33(unsubscribe)))(read(v.subscriptions))();
          write(Nothing.value)(v.subscriptions)();
          bindFlipped6(traverse_33(function() {
            var $60 = killFiber(error("finalized"));
            return function($61) {
              return handleAff($60($61));
            };
          }()))(read(v.forks))();
          return write(empty2)(v.forks)();
        };
      };
      runUI = function(renderSpec2) {
        return function(component2) {
          return function(i2) {
            var squashChildInitializers = function(lchs) {
              return function(preInits) {
                return unDriverStateX(function(st) {
                  var parentInitializer = evalM(render2)(st.selfRef)(st["component"]["eval"](new Initialize(unit)));
                  return modify_(function(handlers) {
                    return {
                      initializers: new Cons(discard22(parSequence_3(reverse2(handlers.initializers)))(function() {
                        return discard22(parentInitializer)(function() {
                          return liftEffect5(function __do2() {
                            handlePending(st.pendingQueries)();
                            return handlePending(st.pendingOuts)();
                          });
                        });
                      }), preInits),
                      finalizers: handlers.finalizers
                    };
                  })(lchs);
                });
              };
            };
            var runComponent = function(lchs) {
              return function(handler3) {
                return function(j) {
                  return unComponent(function(c) {
                    return function __do2() {
                      var lchs$prime = newLifecycleHandlers();
                      var $$var2 = initDriverState(c)(j)(handler3)(lchs$prime)();
                      var pre2 = read(lchs)();
                      write({
                        initializers: Nil.value,
                        finalizers: pre2.finalizers
                      })(lchs)();
                      bindFlipped6(unDriverStateX(function() {
                        var $62 = render2(lchs);
                        return function($63) {
                          return $62(function(v) {
                            return v.selfRef;
                          }($63));
                        };
                      }()))(read($$var2))();
                      bindFlipped6(squashChildInitializers(lchs)(pre2.initializers))(read($$var2))();
                      return $$var2;
                    };
                  });
                };
              };
            };
            var renderChild = function(lchs) {
              return function(handler3) {
                return function(childrenInRef) {
                  return function(childrenOutRef) {
                    return unComponentSlot(function(slot) {
                      return function __do2() {
                        var childrenIn = map16(slot.pop)(read(childrenInRef))();
                        var $$var2 = function() {
                          if (childrenIn instanceof Just) {
                            write(childrenIn.value0.value1)(childrenInRef)();
                            var dsx = read(childrenIn.value0.value0)();
                            unDriverStateX(function(st) {
                              return function __do3() {
                                flip(write)(st.handlerRef)(function() {
                                  var $64 = maybe(pure12(unit))(handler3);
                                  return function($65) {
                                    return $64(slot.output($65));
                                  };
                                }())();
                                return handleAff(evalM(render2)(st.selfRef)(st["component"]["eval"](new Receive(slot.input, unit))))();
                              };
                            })(dsx)();
                            return childrenIn.value0.value0;
                          }
                          ;
                          if (childrenIn instanceof Nothing) {
                            return runComponent(lchs)(function() {
                              var $66 = maybe(pure12(unit))(handler3);
                              return function($67) {
                                return $66(slot.output($67));
                              };
                            }())(slot.input)(slot.component)();
                          }
                          ;
                          throw new Error("Failed pattern match at Halogen.Aff.Driver (line 213, column 14 - line 222, column 98): " + [childrenIn.constructor.name]);
                        }();
                        var isDuplicate = map16(function($68) {
                          return isJust(slot.get($68));
                        })(read(childrenOutRef))();
                        when2(isDuplicate)(warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                        modify_(slot.set($$var2))(childrenOutRef)();
                        return bind4(read($$var2))(renderStateX2(function(v) {
                          if (v instanceof Nothing) {
                            return $$throw("Halogen internal error: child was not initialized in renderChild");
                          }
                          ;
                          if (v instanceof Just) {
                            return pure7(renderSpec2.renderChild(v.value0));
                          }
                          ;
                          throw new Error("Failed pattern match at Halogen.Aff.Driver (line 227, column 37 - line 229, column 50): " + [v.constructor.name]);
                        }))();
                      };
                    });
                  };
                };
              };
            };
            var render2 = function(lchs) {
              return function($$var2) {
                return function __do2() {
                  var v = read($$var2)();
                  var shouldProcessHandlers = map16(isNothing)(read(v.pendingHandlers))();
                  when2(shouldProcessHandlers)(write(new Just(Nil.value))(v.pendingHandlers))();
                  write(empty3)(v.childrenOut)();
                  write(v.children)(v.childrenIn)();
                  var handler3 = function() {
                    var $69 = queueOrRun(v.pendingHandlers);
                    var $70 = evalF(render2)(v.selfRef);
                    return function($71) {
                      return $69($$void5($70($71)));
                    };
                  }();
                  var childHandler = function() {
                    var $72 = queueOrRun(v.pendingQueries);
                    return function($73) {
                      return $72(handler3(Action.create($73)));
                    };
                  }();
                  var rendering = renderSpec2.render(function($74) {
                    return handleAff(handler3($74));
                  })(renderChild(lchs)(childHandler)(v.childrenIn)(v.childrenOut))(v.component.render(v.state))(v.rendering)();
                  var children2 = read(v.childrenOut)();
                  var childrenIn = read(v.childrenIn)();
                  foreachSlot2(childrenIn)(function(v1) {
                    return function __do3() {
                      var childDS = read(v1)();
                      renderStateX_2(renderSpec2.removeChild)(childDS)();
                      return finalize(lchs)(childDS)();
                    };
                  })();
                  flip(modify_)(v.selfRef)(mapDriverState(function(ds$prime) {
                    return {
                      component: ds$prime.component,
                      state: ds$prime.state,
                      refs: ds$prime.refs,
                      childrenIn: ds$prime.childrenIn,
                      childrenOut: ds$prime.childrenOut,
                      selfRef: ds$prime.selfRef,
                      handlerRef: ds$prime.handlerRef,
                      pendingQueries: ds$prime.pendingQueries,
                      pendingOuts: ds$prime.pendingOuts,
                      pendingHandlers: ds$prime.pendingHandlers,
                      fresh: ds$prime.fresh,
                      subscriptions: ds$prime.subscriptions,
                      forks: ds$prime.forks,
                      lifecycleHandlers: ds$prime.lifecycleHandlers,
                      rendering: new Just(rendering),
                      children: children2
                    };
                  }))();
                  return when2(shouldProcessHandlers)(flip(tailRecM3)(unit)(function(v1) {
                    return function __do3() {
                      var handlers = read(v.pendingHandlers)();
                      write(new Just(Nil.value))(v.pendingHandlers)();
                      traverse_23(function() {
                        var $75 = traverse_5(fork4);
                        return function($76) {
                          return handleAff($75(reverse2($76)));
                        };
                      }())(handlers)();
                      var mmore = read(v.pendingHandlers)();
                      var $51 = maybe(false)($$null)(mmore);
                      if ($51) {
                        return voidLeft3(write(Nothing.value)(v.pendingHandlers))(new Done(unit))();
                      }
                      ;
                      return new Loop(unit);
                    };
                  }))();
                };
              };
            };
            var finalize = function(lchs) {
              return unDriverStateX(function(st) {
                return function __do2() {
                  cleanupSubscriptionsAndForks(st)();
                  var f = evalM(render2)(st.selfRef)(st["component"]["eval"](new Finalize(unit)));
                  modify_(function(handlers) {
                    return {
                      initializers: handlers.initializers,
                      finalizers: new Cons(f, handlers.finalizers)
                    };
                  })(lchs)();
                  return foreachSlot2(st.children)(function(v) {
                    return function __do3() {
                      var dsx = read(v)();
                      return finalize(lchs)(dsx)();
                    };
                  })();
                };
              });
            };
            var evalDriver = function(disposed) {
              return function(ref2) {
                return function(q2) {
                  return bind13(liftEffect5(read(disposed)))(function(v) {
                    if (v) {
                      return pure12(Nothing.value);
                    }
                    ;
                    return evalQ(render2)(ref2)(q2);
                  });
                };
              };
            };
            var dispose = function(disposed) {
              return function(lchs) {
                return function(dsx) {
                  return handleLifecycle(lchs)(function __do2() {
                    var v = read(disposed)();
                    if (v) {
                      return unit;
                    }
                    ;
                    write(true)(disposed)();
                    finalize(lchs)(dsx)();
                    return unDriverStateX(function(v1) {
                      return function __do3() {
                        var v2 = liftEffect1(read(v1.selfRef))();
                        return for_2(v2.rendering)(renderSpec2.dispose)();
                      };
                    })(dsx)();
                  });
                };
              };
            };
            return bind13(liftEffect5(newLifecycleHandlers))(function(lchs) {
              return bind13(liftEffect5($$new(false)))(function(disposed) {
                return handleLifecycle(lchs)(function __do2() {
                  var sio = create3();
                  var dsx = bindFlipped6(read)(runComponent(lchs)(function() {
                    var $77 = notify(sio.listener);
                    return function($78) {
                      return liftEffect5($77($78));
                    };
                  }())(i2)(component2))();
                  return unDriverStateX(function(st) {
                    return pure7({
                      query: evalDriver(disposed)(st.selfRef),
                      messages: sio.emitter,
                      dispose: dispose(disposed)(lchs)(dsx)
                    });
                  })(dsx)();
                });
              });
            });
          };
        };
      };
    }
  });

  // output/Web.DOM.Node/foreign.js
  function insertBefore(node1) {
    return function(node2) {
      return function(parent2) {
        return function() {
          parent2.insertBefore(node1, node2);
        };
      };
    };
  }
  function appendChild(node) {
    return function(parent2) {
      return function() {
        parent2.appendChild(node);
      };
    };
  }
  function removeChild2(node) {
    return function(parent2) {
      return function() {
        parent2.removeChild(node);
      };
    };
  }
  var getEffProp2, baseURI, _ownerDocument, _parentNode, _parentElement, childNodes, _firstChild, _lastChild, _previousSibling, _nextSibling, _nodeValue, textContent;
  var init_foreign114 = __esm({
    "output/Web.DOM.Node/foreign.js"() {
      getEffProp2 = function(name15) {
        return function(node) {
          return function() {
            return node[name15];
          };
        };
      };
      baseURI = getEffProp2("baseURI");
      _ownerDocument = getEffProp2("ownerDocument");
      _parentNode = getEffProp2("parentNode");
      _parentElement = getEffProp2("parentElement");
      childNodes = getEffProp2("childNodes");
      _firstChild = getEffProp2("firstChild");
      _lastChild = getEffProp2("lastChild");
      _previousSibling = getEffProp2("previousSibling");
      _nextSibling = getEffProp2("nextSibling");
      _nodeValue = getEffProp2("nodeValue");
      textContent = getEffProp2("textContent");
    }
  });

  // output/Web.DOM.NodeType/index.js
  var init_Web_DOM4 = __esm({
    "output/Web.DOM.NodeType/index.js"() {
      init_Data37();
      init_Data15();
      init_Data12();
    }
  });

  // output/Web.DOM.Node/index.js
  var map17, parentNode2, nextSibling;
  var init_Web_DOM5 = __esm({
    "output/Web.DOM.Node/index.js"() {
      init_foreign114();
      init_Data37();
      init_Data4();
      init_Data15();
      init_Data36();
      init_Effect();
      init_Unsafe();
      init_Web_DOM4();
      init_Web_Internal();
      init_foreign114();
      map17 = /* @__PURE__ */ map(functorEffect);
      parentNode2 = /* @__PURE__ */ function() {
        var $6 = map17(toMaybe);
        return function($7) {
          return $6(_parentNode($7));
        };
      }();
      nextSibling = /* @__PURE__ */ function() {
        var $15 = map17(toMaybe);
        return function($16) {
          return $15(_nextSibling($16));
        };
      }();
    }
  });

  // output/Halogen.VDom.Driver/index.js
  var $runtime_lazy7, $$void6, pure8, traverse_6, unwrap2, when3, not2, identity7, bind14, liftEffect6, map18, bindFlipped7, substInParent, removeChild3, mkSpec, renderSpec, runUI2;
  var init_Halogen_VDom6 = __esm({
    "output/Halogen.VDom.Driver/index.js"() {
      init_Control4();
      init_Control5();
      init_Control2();
      init_Data26();
      init_Data4();
      init_Data21();
      init_Data15();
      init_Data25();
      init_Data3();
      init_Effect();
      init_Effect6();
      init_Effect4();
      init_Effect3();
      init_Halogen_Aff2();
      init_Halogen_Aff_Driver();
      init_Halogen3();
      init_Halogen_VDom4();
      init_Halogen_VDom_DOM();
      init_Halogen_VDom();
      init_Halogen_VDom5();
      init_Unsafe2();
      init_Web_DOM5();
      init_Web();
      init_Web_HTML14();
      init_Web_HTML15();
      init_Web_HTML69();
      $runtime_lazy7 = function(name15, moduleName, init2) {
        var state3 = 0;
        var val;
        return function(lineNumber) {
          if (state3 === 2)
            return val;
          if (state3 === 1)
            throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
          state3 = 1;
          val = init2();
          state3 = 2;
          return val;
        };
      };
      $$void6 = /* @__PURE__ */ $$void(functorEffect);
      pure8 = /* @__PURE__ */ pure(applicativeEffect);
      traverse_6 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
      unwrap2 = /* @__PURE__ */ unwrap();
      when3 = /* @__PURE__ */ when(applicativeEffect);
      not2 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean)));
      identity7 = /* @__PURE__ */ identity(categoryFn);
      bind14 = /* @__PURE__ */ bind(bindAff);
      liftEffect6 = /* @__PURE__ */ liftEffect(monadEffectAff);
      map18 = /* @__PURE__ */ map(functorEffect);
      bindFlipped7 = /* @__PURE__ */ bindFlipped(bindEffect);
      substInParent = function(v) {
        return function(v1) {
          return function(v2) {
            if (v1 instanceof Just && v2 instanceof Just) {
              return $$void6(insertBefore(v)(v1.value0)(v2.value0));
            }
            ;
            if (v1 instanceof Nothing && v2 instanceof Just) {
              return $$void6(appendChild(v)(v2.value0));
            }
            ;
            return pure8(unit);
          };
        };
      };
      removeChild3 = function(v) {
        return function __do2() {
          var npn = parentNode2(v.node)();
          return traverse_6(function(pn) {
            return removeChild2(v.node)(pn);
          })(npn)();
        };
      };
      mkSpec = function(handler3) {
        return function(renderChildRef) {
          return function(document2) {
            var getNode = unRenderStateX(function(v) {
              return v.node;
            });
            var done = function(st) {
              if (st instanceof Just) {
                return halt(st.value0);
              }
              ;
              return unit;
            };
            var buildWidget2 = function(spec) {
              var buildThunk2 = buildThunk(unwrap2)(spec);
              var $lazy_patch = $runtime_lazy7("patch", "Halogen.VDom.Driver", function() {
                return function(st, slot) {
                  if (st instanceof Just) {
                    if (slot instanceof ComponentSlot) {
                      halt(st.value0);
                      return $lazy_renderComponentSlot(100)(slot.value0);
                    }
                    ;
                    if (slot instanceof ThunkSlot) {
                      var step$prime = step3(st.value0, slot.value0);
                      return mkStep(new Step(extract2(step$prime), new Just(step$prime), $lazy_patch(103), done));
                    }
                    ;
                    throw new Error("Failed pattern match at Halogen.VDom.Driver (line 97, column 22 - line 103, column 79): " + [slot.constructor.name]);
                  }
                  ;
                  return $lazy_render(104)(slot);
                };
              });
              var $lazy_render = $runtime_lazy7("render", "Halogen.VDom.Driver", function() {
                return function(slot) {
                  if (slot instanceof ComponentSlot) {
                    return $lazy_renderComponentSlot(86)(slot.value0);
                  }
                  ;
                  if (slot instanceof ThunkSlot) {
                    var step4 = buildThunk2(slot.value0);
                    return mkStep(new Step(extract2(step4), new Just(step4), $lazy_patch(89), done));
                  }
                  ;
                  throw new Error("Failed pattern match at Halogen.VDom.Driver (line 84, column 7 - line 89, column 75): " + [slot.constructor.name]);
                };
              });
              var $lazy_renderComponentSlot = $runtime_lazy7("renderComponentSlot", "Halogen.VDom.Driver", function() {
                return function(cs) {
                  var renderChild = read(renderChildRef)();
                  var rsx = renderChild(cs)();
                  var node = getNode(rsx);
                  return mkStep(new Step(node, Nothing.value, $lazy_patch(117), done));
                };
              });
              var patch = $lazy_patch(91);
              var render2 = $lazy_render(82);
              var renderComponentSlot = $lazy_renderComponentSlot(109);
              return render2;
            };
            var buildAttributes = buildProp(handler3);
            return {
              buildWidget: buildWidget2,
              buildAttributes,
              document: document2
            };
          };
        };
      };
      renderSpec = function(document2) {
        return function(container) {
          var render2 = function(handler3) {
            return function(child) {
              return function(v) {
                return function(v1) {
                  if (v1 instanceof Nothing) {
                    return function __do2() {
                      var renderChildRef = $$new(child)();
                      var spec = mkSpec(handler3)(renderChildRef)(document2);
                      var machine = buildVDom(spec)(v);
                      var node = extract2(machine);
                      $$void6(appendChild(node)(toNode(container)))();
                      return {
                        machine,
                        node,
                        renderChildRef
                      };
                    };
                  }
                  ;
                  if (v1 instanceof Just) {
                    return function __do2() {
                      write(child)(v1.value0.renderChildRef)();
                      var parent2 = parentNode2(v1.value0.node)();
                      var nextSib = nextSibling(v1.value0.node)();
                      var machine$prime = step3(v1.value0.machine, v);
                      var newNode = extract2(machine$prime);
                      when3(not2(unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(nextSib)(parent2))();
                      return {
                        machine: machine$prime,
                        node: newNode,
                        renderChildRef: v1.value0.renderChildRef
                      };
                    };
                  }
                  ;
                  throw new Error("Failed pattern match at Halogen.VDom.Driver (line 157, column 5 - line 173, column 80): " + [v1.constructor.name]);
                };
              };
            };
          };
          return {
            render: render2,
            renderChild: identity7,
            removeChild: removeChild3,
            dispose: removeChild3
          };
        };
      };
      runUI2 = function(component2) {
        return function(i2) {
          return function(element3) {
            return bind14(liftEffect6(map18(toDocument)(bindFlipped7(document)(windowImpl))))(function(document2) {
              return runUI(renderSpec(document2)(element3))(component2)(i2);
            });
          };
        };
      };
    }
  });

  // output/Data.Eq.Generic/index.js
  var init_Data_Eq = __esm({
    "output/Data.Eq.Generic/index.js"() {
      init_Data8();
      init_Data_Generic();
    }
  });

  // output/Data.Show.Generic/foreign.js
  var intercalate2;
  var init_foreign115 = __esm({
    "output/Data.Show.Generic/foreign.js"() {
      intercalate2 = function(separator) {
        return function(xs) {
          return xs.join(separator);
        };
      };
    }
  });

  // output/Data.Show.Generic/index.js
  var append5, genericShowArgsNoArguments, genericShowArgsArgument, genericShowArgs, genericShowArgsProduct, genericShowConstructor, genericShow$prime, genericShowSum, genericShow;
  var init_Data_Show = __esm({
    "output/Data.Show.Generic/index.js"() {
      init_foreign115();
      init_Data_Generic();
      init_Data7();
      init_Data14();
      init_Data5();
      init_Type();
      append5 = /* @__PURE__ */ append(semigroupArray);
      genericShowArgsNoArguments = {
        genericShowArgs: function(v) {
          return [];
        }
      };
      genericShowArgsArgument = function(dictShow) {
        var show3 = show(dictShow);
        return {
          genericShowArgs: function(v) {
            return [show3(v)];
          }
        };
      };
      genericShowArgs = function(dict) {
        return dict.genericShowArgs;
      };
      genericShowArgsProduct = function(dictGenericShowArgs) {
        var genericShowArgs1 = genericShowArgs(dictGenericShowArgs);
        return function(dictGenericShowArgs1) {
          var genericShowArgs2 = genericShowArgs(dictGenericShowArgs1);
          return {
            genericShowArgs: function(v) {
              return append5(genericShowArgs1(v.value0))(genericShowArgs2(v.value1));
            }
          };
        };
      };
      genericShowConstructor = function(dictGenericShowArgs) {
        var genericShowArgs1 = genericShowArgs(dictGenericShowArgs);
        return function(dictIsSymbol) {
          var reflectSymbol2 = reflectSymbol(dictIsSymbol);
          return {
            "genericShow'": function(v) {
              var ctor = reflectSymbol2($$Proxy.value);
              var v1 = genericShowArgs1(v);
              if (v1.length === 0) {
                return ctor;
              }
              ;
              return "(" + (intercalate2(" ")(append5([ctor])(v1)) + ")");
            }
          };
        };
      };
      genericShow$prime = function(dict) {
        return dict["genericShow'"];
      };
      genericShowSum = function(dictGenericShow) {
        var genericShow$prime1 = genericShow$prime(dictGenericShow);
        return function(dictGenericShow1) {
          var genericShow$prime2 = genericShow$prime(dictGenericShow1);
          return {
            "genericShow'": function(v) {
              if (v instanceof Inl) {
                return genericShow$prime1(v.value0);
              }
              ;
              if (v instanceof Inr) {
                return genericShow$prime2(v.value0);
              }
              ;
              throw new Error("Failed pattern match at Data.Show.Generic (line 26, column 1 - line 28, column 40): " + [v.constructor.name]);
            }
          };
        };
      };
      genericShow = function(dictGeneric) {
        var from3 = from(dictGeneric);
        return function(dictGenericShow) {
          var genericShow$prime1 = genericShow$prime(dictGenericShow);
          return function(x) {
            return genericShow$prime1(from3(x));
          };
        };
      };
    }
  });

  // output/MidiTypes/index.js
  var showRecord2, chanIsSymbol, showRecordFieldsCons2, keyIsSymbol, velIsSymbol, genericShowConstructor2, genericShowConstructor1, ctrlIsSymbol, valIsSymbol, showRecordFieldsConsNil2, progNumIsSymbol, posIsSymbol, genericShowArgsArgument2, genericShowConstructor22, genericShowConstructor3, fFrIsSymbol, frIsSymbol, hrIsSymbol, mnIsSymbol, secIsSymbol, bbIsSymbol, ccIsSymbol, ddIsSymbol, nnIsSymbol, miIsSymbol, sfIsSymbol, SeqNum, Text2, Copyright, TrackName, InstName, Lyric, Marker, CuePoint, ChannelPrefix, EndOfTrack, Tempo, SmpteOffset, TimeSigEv, KeySigEv, SeqSpec, UnknownMeta, NoteOff, NoteOn, PolyKeyPress, CC, ProgChange, AfterTouch, PitchWheel, ChanMode, MidiEvent, MetaEvent, genericMidiEvent_, showMidiEvent, genericMetaEvent_, showMetaEvent, genericEvent_, showEvent;
  var init_MidiTypes = __esm({
    "output/MidiTypes/index.js"() {
      init_Data8();
      init_Data_Eq();
      init_Data_Generic();
      init_Data14();
      init_Data_Show();
      showRecord2 = /* @__PURE__ */ showRecord()();
      chanIsSymbol = {
        reflectSymbol: function() {
          return "chan";
        }
      };
      showRecordFieldsCons2 = /* @__PURE__ */ showRecordFieldsCons(chanIsSymbol);
      keyIsSymbol = {
        reflectSymbol: function() {
          return "key";
        }
      };
      velIsSymbol = {
        reflectSymbol: function() {
          return "vel";
        }
      };
      genericShowConstructor2 = /* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(/* @__PURE__ */ showRecord2(/* @__PURE__ */ showRecordFieldsCons2(/* @__PURE__ */ showRecordFieldsCons(keyIsSymbol)(/* @__PURE__ */ showRecordFieldsConsNil(velIsSymbol)(showInt))(showInt))(showInt))));
      genericShowConstructor1 = /* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments);
      ctrlIsSymbol = {
        reflectSymbol: function() {
          return "ctrl";
        }
      };
      valIsSymbol = {
        reflectSymbol: function() {
          return "val";
        }
      };
      showRecordFieldsConsNil2 = /* @__PURE__ */ showRecordFieldsConsNil(valIsSymbol)(showInt);
      progNumIsSymbol = {
        reflectSymbol: function() {
          return "progNum";
        }
      };
      posIsSymbol = {
        reflectSymbol: function() {
          return "pos";
        }
      };
      genericShowArgsArgument2 = /* @__PURE__ */ genericShowArgsArgument(showInt);
      genericShowConstructor22 = /* @__PURE__ */ genericShowConstructor(genericShowArgsArgument2);
      genericShowConstructor3 = /* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(showString));
      fFrIsSymbol = {
        reflectSymbol: function() {
          return "fFr";
        }
      };
      frIsSymbol = {
        reflectSymbol: function() {
          return "fr";
        }
      };
      hrIsSymbol = {
        reflectSymbol: function() {
          return "hr";
        }
      };
      mnIsSymbol = {
        reflectSymbol: function() {
          return "mn";
        }
      };
      secIsSymbol = {
        reflectSymbol: function() {
          return "sec";
        }
      };
      bbIsSymbol = {
        reflectSymbol: function() {
          return "bb";
        }
      };
      ccIsSymbol = {
        reflectSymbol: function() {
          return "cc";
        }
      };
      ddIsSymbol = {
        reflectSymbol: function() {
          return "dd";
        }
      };
      nnIsSymbol = {
        reflectSymbol: function() {
          return "nn";
        }
      };
      miIsSymbol = {
        reflectSymbol: function() {
          return "mi";
        }
      };
      sfIsSymbol = {
        reflectSymbol: function() {
          return "sf";
        }
      };
      SeqNum = /* @__PURE__ */ function() {
        function SeqNum2(value0) {
          this.value0 = value0;
        }
        ;
        SeqNum2.create = function(value0) {
          return new SeqNum2(value0);
        };
        return SeqNum2;
      }();
      Text2 = /* @__PURE__ */ function() {
        function Text3(value0) {
          this.value0 = value0;
        }
        ;
        Text3.create = function(value0) {
          return new Text3(value0);
        };
        return Text3;
      }();
      Copyright = /* @__PURE__ */ function() {
        function Copyright2(value0) {
          this.value0 = value0;
        }
        ;
        Copyright2.create = function(value0) {
          return new Copyright2(value0);
        };
        return Copyright2;
      }();
      TrackName = /* @__PURE__ */ function() {
        function TrackName2(value0) {
          this.value0 = value0;
        }
        ;
        TrackName2.create = function(value0) {
          return new TrackName2(value0);
        };
        return TrackName2;
      }();
      InstName = /* @__PURE__ */ function() {
        function InstName2(value0) {
          this.value0 = value0;
        }
        ;
        InstName2.create = function(value0) {
          return new InstName2(value0);
        };
        return InstName2;
      }();
      Lyric = /* @__PURE__ */ function() {
        function Lyric2(value0) {
          this.value0 = value0;
        }
        ;
        Lyric2.create = function(value0) {
          return new Lyric2(value0);
        };
        return Lyric2;
      }();
      Marker = /* @__PURE__ */ function() {
        function Marker2(value0) {
          this.value0 = value0;
        }
        ;
        Marker2.create = function(value0) {
          return new Marker2(value0);
        };
        return Marker2;
      }();
      CuePoint = /* @__PURE__ */ function() {
        function CuePoint2(value0) {
          this.value0 = value0;
        }
        ;
        CuePoint2.create = function(value0) {
          return new CuePoint2(value0);
        };
        return CuePoint2;
      }();
      ChannelPrefix = /* @__PURE__ */ function() {
        function ChannelPrefix2(value0) {
          this.value0 = value0;
        }
        ;
        ChannelPrefix2.create = function(value0) {
          return new ChannelPrefix2(value0);
        };
        return ChannelPrefix2;
      }();
      EndOfTrack = /* @__PURE__ */ function() {
        function EndOfTrack2() {
        }
        ;
        EndOfTrack2.value = new EndOfTrack2();
        return EndOfTrack2;
      }();
      Tempo = /* @__PURE__ */ function() {
        function Tempo2(value0) {
          this.value0 = value0;
        }
        ;
        Tempo2.create = function(value0) {
          return new Tempo2(value0);
        };
        return Tempo2;
      }();
      SmpteOffset = /* @__PURE__ */ function() {
        function SmpteOffset2(value0) {
          this.value0 = value0;
        }
        ;
        SmpteOffset2.create = function(value0) {
          return new SmpteOffset2(value0);
        };
        return SmpteOffset2;
      }();
      TimeSigEv = /* @__PURE__ */ function() {
        function TimeSigEv2(value0) {
          this.value0 = value0;
        }
        ;
        TimeSigEv2.create = function(value0) {
          return new TimeSigEv2(value0);
        };
        return TimeSigEv2;
      }();
      KeySigEv = /* @__PURE__ */ function() {
        function KeySigEv2(value0) {
          this.value0 = value0;
        }
        ;
        KeySigEv2.create = function(value0) {
          return new KeySigEv2(value0);
        };
        return KeySigEv2;
      }();
      SeqSpec = /* @__PURE__ */ function() {
        function SeqSpec2() {
        }
        ;
        SeqSpec2.value = new SeqSpec2();
        return SeqSpec2;
      }();
      UnknownMeta = /* @__PURE__ */ function() {
        function UnknownMeta2() {
        }
        ;
        UnknownMeta2.value = new UnknownMeta2();
        return UnknownMeta2;
      }();
      NoteOff = /* @__PURE__ */ function() {
        function NoteOff2(value0) {
          this.value0 = value0;
        }
        ;
        NoteOff2.create = function(value0) {
          return new NoteOff2(value0);
        };
        return NoteOff2;
      }();
      NoteOn = /* @__PURE__ */ function() {
        function NoteOn2(value0) {
          this.value0 = value0;
        }
        ;
        NoteOn2.create = function(value0) {
          return new NoteOn2(value0);
        };
        return NoteOn2;
      }();
      PolyKeyPress = /* @__PURE__ */ function() {
        function PolyKeyPress2() {
        }
        ;
        PolyKeyPress2.value = new PolyKeyPress2();
        return PolyKeyPress2;
      }();
      CC = /* @__PURE__ */ function() {
        function CC2(value0) {
          this.value0 = value0;
        }
        ;
        CC2.create = function(value0) {
          return new CC2(value0);
        };
        return CC2;
      }();
      ProgChange = /* @__PURE__ */ function() {
        function ProgChange2(value0) {
          this.value0 = value0;
        }
        ;
        ProgChange2.create = function(value0) {
          return new ProgChange2(value0);
        };
        return ProgChange2;
      }();
      AfterTouch = /* @__PURE__ */ function() {
        function AfterTouch2(value0) {
          this.value0 = value0;
        }
        ;
        AfterTouch2.create = function(value0) {
          return new AfterTouch2(value0);
        };
        return AfterTouch2;
      }();
      PitchWheel = /* @__PURE__ */ function() {
        function PitchWheel2(value0) {
          this.value0 = value0;
        }
        ;
        PitchWheel2.create = function(value0) {
          return new PitchWheel2(value0);
        };
        return PitchWheel2;
      }();
      ChanMode = /* @__PURE__ */ function() {
        function ChanMode2() {
        }
        ;
        ChanMode2.value = new ChanMode2();
        return ChanMode2;
      }();
      MidiEvent = /* @__PURE__ */ function() {
        function MidiEvent2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        MidiEvent2.create = function(value0) {
          return function(value1) {
            return new MidiEvent2(value0, value1);
          };
        };
        return MidiEvent2;
      }();
      MetaEvent = /* @__PURE__ */ function() {
        function MetaEvent2(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
        }
        ;
        MetaEvent2.create = function(value0) {
          return function(value1) {
            return new MetaEvent2(value0, value1);
          };
        };
        return MetaEvent2;
      }();
      genericMidiEvent_ = {
        to: function(x) {
          if (x instanceof Inl) {
            return new NoteOff(x.value0);
          }
          ;
          if (x instanceof Inr && x.value0 instanceof Inl) {
            return new NoteOn(x.value0.value0);
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && x.value0.value0 instanceof Inl)) {
            return PolyKeyPress.value;
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && x.value0.value0.value0 instanceof Inl))) {
            return new CC(x.value0.value0.value0.value0);
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0 instanceof Inl)))) {
            return new ProgChange(x.value0.value0.value0.value0.value0);
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0 instanceof Inl))))) {
            return new AfterTouch(x.value0.value0.value0.value0.value0.value0);
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0 instanceof Inl)))))) {
            return new PitchWheel(x.value0.value0.value0.value0.value0.value0.value0);
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0 instanceof Inr)))))) {
            return ChanMode.value;
          }
          ;
          throw new Error("Failed pattern match at MidiTypes (line 111, column 1 - line 111, column 36): " + [x.constructor.name]);
        },
        from: function(x) {
          if (x instanceof NoteOff) {
            return new Inl(x.value0);
          }
          ;
          if (x instanceof NoteOn) {
            return new Inr(new Inl(x.value0));
          }
          ;
          if (x instanceof PolyKeyPress) {
            return new Inr(new Inr(new Inl(NoArguments.value)));
          }
          ;
          if (x instanceof CC) {
            return new Inr(new Inr(new Inr(new Inl(x.value0))));
          }
          ;
          if (x instanceof ProgChange) {
            return new Inr(new Inr(new Inr(new Inr(new Inl(x.value0)))));
          }
          ;
          if (x instanceof AfterTouch) {
            return new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(x.value0))))));
          }
          ;
          if (x instanceof PitchWheel) {
            return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(x.value0)))))));
          }
          ;
          if (x instanceof ChanMode) {
            return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(NoArguments.value)))))));
          }
          ;
          throw new Error("Failed pattern match at MidiTypes (line 111, column 1 - line 111, column 36): " + [x.constructor.name]);
        }
      };
      showMidiEvent = {
        show: /* @__PURE__ */ genericShow(genericMidiEvent_)(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
          reflectSymbol: function() {
            return "NoteOff";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
          reflectSymbol: function() {
            return "NoteOn";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor1({
          reflectSymbol: function() {
            return "PolyKeyPress";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(/* @__PURE__ */ showRecord2(/* @__PURE__ */ showRecordFieldsCons2(/* @__PURE__ */ showRecordFieldsCons(ctrlIsSymbol)(showRecordFieldsConsNil2)(showInt))(showInt))))({
          reflectSymbol: function() {
            return "CC";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(/* @__PURE__ */ showRecord2(/* @__PURE__ */ showRecordFieldsCons2(/* @__PURE__ */ showRecordFieldsConsNil(progNumIsSymbol)(showInt))(showInt))))({
          reflectSymbol: function() {
            return "ProgChange";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(/* @__PURE__ */ showRecord2(/* @__PURE__ */ showRecordFieldsCons2(showRecordFieldsConsNil2)(showInt))))({
          reflectSymbol: function() {
            return "AfterTouch";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(/* @__PURE__ */ showRecord2(/* @__PURE__ */ showRecordFieldsCons2(/* @__PURE__ */ showRecordFieldsConsNil(posIsSymbol)(showInt))(showInt))))({
          reflectSymbol: function() {
            return "PitchWheel";
          }
        }))(/* @__PURE__ */ genericShowConstructor1({
          reflectSymbol: function() {
            return "ChanMode";
          }
        })))))))))
      };
      genericMetaEvent_ = {
        to: function(x) {
          if (x instanceof Inl) {
            return new SeqNum(x.value0);
          }
          ;
          if (x instanceof Inr && x.value0 instanceof Inl) {
            return new Text2(x.value0.value0);
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && x.value0.value0 instanceof Inl)) {
            return new Copyright(x.value0.value0.value0);
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && x.value0.value0.value0 instanceof Inl))) {
            return new TrackName(x.value0.value0.value0.value0);
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0 instanceof Inl)))) {
            return new InstName(x.value0.value0.value0.value0.value0);
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0 instanceof Inl))))) {
            return new Lyric(x.value0.value0.value0.value0.value0.value0);
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0 instanceof Inl)))))) {
            return new Marker(x.value0.value0.value0.value0.value0.value0.value0);
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))) {
            return new CuePoint(x.value0.value0.value0.value0.value0.value0.value0.value0);
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))) {
            return new ChannelPrefix(x.value0.value0.value0.value0.value0.value0.value0.value0.value0);
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))))) {
            return EndOfTrack.value;
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))))) {
            return new Tempo(x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0);
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))))))) {
            return new SmpteOffset(x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0);
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))))))) {
            return new TimeSigEv(x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0);
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))))))))) {
            return new KeySigEv(x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0);
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))))))))) {
            return SeqSpec.value;
          }
          ;
          if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr)))))))))))))) {
            return UnknownMeta.value;
          }
          ;
          throw new Error("Failed pattern match at MidiTypes (line 118, column 1 - line 118, column 36): " + [x.constructor.name]);
        },
        from: function(x) {
          if (x instanceof SeqNum) {
            return new Inl(x.value0);
          }
          ;
          if (x instanceof Text2) {
            return new Inr(new Inl(x.value0));
          }
          ;
          if (x instanceof Copyright) {
            return new Inr(new Inr(new Inl(x.value0)));
          }
          ;
          if (x instanceof TrackName) {
            return new Inr(new Inr(new Inr(new Inl(x.value0))));
          }
          ;
          if (x instanceof InstName) {
            return new Inr(new Inr(new Inr(new Inr(new Inl(x.value0)))));
          }
          ;
          if (x instanceof Lyric) {
            return new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(x.value0))))));
          }
          ;
          if (x instanceof Marker) {
            return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(x.value0)))))));
          }
          ;
          if (x instanceof CuePoint) {
            return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(x.value0))))))));
          }
          ;
          if (x instanceof ChannelPrefix) {
            return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(x.value0)))))))));
          }
          ;
          if (x instanceof EndOfTrack) {
            return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))))))));
          }
          ;
          if (x instanceof Tempo) {
            return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(x.value0)))))))))));
          }
          ;
          if (x instanceof SmpteOffset) {
            return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(x.value0))))))))))));
          }
          ;
          if (x instanceof TimeSigEv) {
            return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(x.value0)))))))))))));
          }
          ;
          if (x instanceof KeySigEv) {
            return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(x.value0))))))))))))));
          }
          ;
          if (x instanceof SeqSpec) {
            return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))))))))))))));
          }
          ;
          if (x instanceof UnknownMeta) {
            return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(NoArguments.value)))))))))))))));
          }
          ;
          throw new Error("Failed pattern match at MidiTypes (line 118, column 1 - line 118, column 36): " + [x.constructor.name]);
        }
      };
      showMetaEvent = {
        show: /* @__PURE__ */ genericShow(genericMetaEvent_)(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor22({
          reflectSymbol: function() {
            return "SeqNum";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor3({
          reflectSymbol: function() {
            return "Text";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor3({
          reflectSymbol: function() {
            return "Copyright";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor3({
          reflectSymbol: function() {
            return "TrackName";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor3({
          reflectSymbol: function() {
            return "InstName";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor3({
          reflectSymbol: function() {
            return "Lyric";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor3({
          reflectSymbol: function() {
            return "Marker";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor3({
          reflectSymbol: function() {
            return "CuePoint";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor22({
          reflectSymbol: function() {
            return "ChannelPrefix";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor1({
          reflectSymbol: function() {
            return "EndOfTrack";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor22({
          reflectSymbol: function() {
            return "Tempo";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(/* @__PURE__ */ showRecord2(/* @__PURE__ */ showRecordFieldsCons(fFrIsSymbol)(/* @__PURE__ */ showRecordFieldsCons(frIsSymbol)(/* @__PURE__ */ showRecordFieldsCons(hrIsSymbol)(/* @__PURE__ */ showRecordFieldsCons(mnIsSymbol)(/* @__PURE__ */ showRecordFieldsConsNil(secIsSymbol)(showInt))(showInt))(showInt))(showInt))(showInt))))({
          reflectSymbol: function() {
            return "SmpteOffset";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(/* @__PURE__ */ showRecord2(/* @__PURE__ */ showRecordFieldsCons(bbIsSymbol)(/* @__PURE__ */ showRecordFieldsCons(ccIsSymbol)(/* @__PURE__ */ showRecordFieldsCons(ddIsSymbol)(/* @__PURE__ */ showRecordFieldsConsNil(nnIsSymbol)(showInt))(showInt))(showInt))(showInt))))({
          reflectSymbol: function() {
            return "TimeSigEv";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(/* @__PURE__ */ showRecord2(/* @__PURE__ */ showRecordFieldsCons(miIsSymbol)(/* @__PURE__ */ showRecordFieldsConsNil(sfIsSymbol)(showInt))(showInt))))({
          reflectSymbol: function() {
            return "KeySigEv";
          }
        }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor1({
          reflectSymbol: function() {
            return "SeqSpec";
          }
        }))(/* @__PURE__ */ genericShowConstructor1({
          reflectSymbol: function() {
            return "UnknownMeta";
          }
        })))))))))))))))))
      };
      genericEvent_ = {
        to: function(x) {
          if (x instanceof Inl) {
            return new MidiEvent(x.value0.value0, x.value0.value1);
          }
          ;
          if (x instanceof Inr) {
            return new MetaEvent(x.value0.value0, x.value0.value1);
          }
          ;
          throw new Error("Failed pattern match at MidiTypes (line 104, column 1 - line 104, column 32): " + [x.constructor.name]);
        },
        from: function(x) {
          if (x instanceof MidiEvent) {
            return new Inl(new Product(x.value0, x.value1));
          }
          ;
          if (x instanceof MetaEvent) {
            return new Inr(new Product(x.value0, x.value1));
          }
          ;
          throw new Error("Failed pattern match at MidiTypes (line 104, column 1 - line 104, column 32): " + [x.constructor.name]);
        }
      };
      showEvent = {
        show: /* @__PURE__ */ genericShow(genericEvent_)(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsProduct(/* @__PURE__ */ genericShowArgsArgument(showMidiEvent))(genericShowArgsArgument2))({
          reflectSymbol: function() {
            return "MidiEvent";
          }
        }))(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsProduct(/* @__PURE__ */ genericShowArgsArgument(showMetaEvent))(genericShowArgsArgument2))({
          reflectSymbol: function() {
            return "MetaEvent";
          }
        })))
      };
    }
  });

  // output/Bits/index.js
  var unwrap3, empty7, div3, semigroupBits, append6, bitToInt, unsafeBitsToInt, _1, _0, padEight, zero2, intToBits, combine2, combine3, combine4;
  var init_Bits = __esm({
    "output/Bits/index.js"() {
      init_Control9();
      init_Data32();
      init_Data();
      init_Data18();
      init_Data45();
      init_Data25();
      init_Data7();
      init_Data14();
      init_Data22();
      unwrap3 = /* @__PURE__ */ unwrap();
      empty7 = /* @__PURE__ */ empty(plusArray);
      div3 = /* @__PURE__ */ div(euclideanRingInt);
      semigroupBits = semigroupArray;
      append6 = /* @__PURE__ */ append(semigroupBits);
      bitToInt = function(v) {
        if (v) {
          return 1;
        }
        ;
        if (!v) {
          return 0;
        }
        ;
        throw new Error("Failed pattern match at Bits (line 41, column 1 - line 41, column 23): " + [v.constructor.name]);
      };
      unsafeBitsToInt = function(v) {
        var f = function(b2) {
          return function(v1) {
            return new Tuple((v1.value1 * bitToInt(b2) | 0) + v1.value0 | 0, v1.value1 * 2 | 0);
          };
        };
        return fst(foldr2(f)(new Tuple(0, 1))(v));
      };
      _1 = true;
      _0 = false;
      padEight = function(bits) {
        var unwrapped = unwrap3(bits);
        var pad = replicate(8 - length(unwrapped) | 0)(_0);
        return append6(pad)(bits);
      };
      zero2 = [_0];
      intToBits = function(v) {
        if (v === 0) {
          return zero2;
        }
        ;
        var f = function(v1) {
          if (v1 === 0) {
            return empty7;
          }
          ;
          if (odd(v1)) {
            return snoc(f(div3(v1)(2)))(_1);
          }
          ;
          if (otherwise) {
            return snoc(f(div3(v1)(2)))(_0);
          }
          ;
          throw new Error("Failed pattern match at Bits (line 36, column 5 - line 36, column 16): " + [v1.constructor.name]);
        };
        return f(v);
      };
      combine2 = function(byte1) {
        return function(byte2) {
          return unsafeBitsToInt(append6(padEight(intToBits(byte1)))(padEight(intToBits(byte2))));
        };
      };
      combine3 = function(byte1) {
        return function(byte2) {
          return function(byte3) {
            return unsafeBitsToInt(append6(padEight(intToBits(byte1)))(append6(padEight(intToBits(byte2)))(padEight(intToBits(byte3)))));
          };
        };
      };
      combine4 = function(a2) {
        return function(b2) {
          return function(c) {
            return function(d) {
              return unsafeBitsToInt(append6(padEight(intToBits(a2)))(append6(padEight(intToBits(b2)))(append6(padEight(intToBits(c)))(padEight(intToBits(d))))));
            };
          };
        };
      };
    }
  });

  // output/Data.Char/index.js
  var fromCharCode2;
  var init_Data48 = __esm({
    "output/Data.Char/index.js"() {
      init_Data37();
      fromCharCode2 = /* @__PURE__ */ toEnum(boundedEnumChar);
    }
  });

  // output/Data.Int.Bits/foreign.js
  var and2;
  var init_foreign116 = __esm({
    "output/Data.Int.Bits/foreign.js"() {
      and2 = function(n1) {
        return function(n2) {
          return n1 & n2;
        };
      };
    }
  });

  // output/Data.Int.Bits/index.js
  var init_Data_Int = __esm({
    "output/Data.Int.Bits/index.js"() {
      init_foreign116();
      init_foreign116();
    }
  });

  // output/StateParser/index.js
  var monadExceptT2, bindStateT2, bind5, monadStateStateT2, get2, discard5, put2, applicativeStateT2, pure9, lift4, monadThrowExceptT2, throwError3, sequence2, throwError1, functorStateT2, mapFlipped2, map19, unless2, eqArray2, eq2, when4, show1, fromFoldable3, ParseError, MissingMThd, MissingMTrk, VarLenExccededMaxDepth, InvalidHeaderLengthVal, UnspecifiedMidiEvent, UnspecifiedMetaEvent, NoError, semigroupParseError, alt5, monoidParseError, readInt2, readManyInts, readInt16, readInt32, peekInt, parseVarLen, parseTextEvent, parseSeqSpec, parseProgChange, parsePitchWheel, parseNoteOn, parseNoteOff, parseHeader, parseCC, parseAfterTouch, parseMidiEvent, many, assertNext, parseChanPrefix, parseEndOfTrack, parseKeySig, parseSMTPEOffset, parseSeqNum, parseTempo, parseTimeSig, parseMetaEvent, parseTrackEvent, parseTrack, parseFile;
  var init_StateParser = __esm({
    "output/StateParser/index.js"() {
      init_Bits();
      init_Control7();
      init_Control4();
      init_Control5();
      init_Control_Monad_Error();
      init_Control_Monad_Except();
      init_Control_Monad_Rec();
      init_Control_Monad_State();
      init_Control_Monad_State2();
      init_Control_Monad_Trans();
      init_Data32();
      init_Data48();
      init_Data8();
      init_Data_Eq();
      init_Data2();
      init_Data4();
      init_Data_Generic();
      init_Data20();
      init_Data45();
      init_Data_Int();
      init_Data43();
      init_Data_List();
      init_Data15();
      init_Data44();
      init_Data14();
      init_Data_Show();
      init_Data_String3();
      init_Data29();
      init_MidiTypes();
      monadExceptT2 = /* @__PURE__ */ monadExceptT(monadIdentity);
      bindStateT2 = /* @__PURE__ */ bindStateT(monadExceptT2);
      bind5 = /* @__PURE__ */ bind(bindStateT2);
      monadStateStateT2 = /* @__PURE__ */ monadStateStateT(monadExceptT2);
      get2 = /* @__PURE__ */ get(monadStateStateT2);
      discard5 = /* @__PURE__ */ discard(discardUnit)(bindStateT2);
      put2 = /* @__PURE__ */ put(monadStateStateT2);
      applicativeStateT2 = /* @__PURE__ */ applicativeStateT(monadExceptT2);
      pure9 = /* @__PURE__ */ pure(applicativeStateT2);
      lift4 = /* @__PURE__ */ lift(monadTransStateT)(monadExceptT2);
      monadThrowExceptT2 = /* @__PURE__ */ monadThrowExceptT(monadIdentity);
      throwError3 = /* @__PURE__ */ throwError(monadThrowExceptT2);
      sequence2 = /* @__PURE__ */ sequence(traversableArray)(applicativeStateT2);
      throwError1 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowStateT(monadThrowExceptT2));
      functorStateT2 = /* @__PURE__ */ functorStateT(/* @__PURE__ */ functorExceptT(functorIdentity));
      mapFlipped2 = /* @__PURE__ */ mapFlipped(functorStateT2);
      map19 = /* @__PURE__ */ map(functorStateT2);
      unless2 = /* @__PURE__ */ unless(applicativeStateT2);
      eqArray2 = /* @__PURE__ */ eqArray(eqInt);
      eq2 = /* @__PURE__ */ eq(eqArray2);
      when4 = /* @__PURE__ */ when(applicativeStateT2);
      show1 = /* @__PURE__ */ show(showInt);
      fromFoldable3 = /* @__PURE__ */ fromFoldable(foldableList);
      ParseError = /* @__PURE__ */ function() {
        function ParseError2(value0) {
          this.value0 = value0;
        }
        ;
        ParseError2.create = function(value0) {
          return new ParseError2(value0);
        };
        return ParseError2;
      }();
      MissingMThd = /* @__PURE__ */ function() {
        function MissingMThd2() {
        }
        ;
        MissingMThd2.value = new MissingMThd2();
        return MissingMThd2;
      }();
      MissingMTrk = /* @__PURE__ */ function() {
        function MissingMTrk2() {
        }
        ;
        MissingMTrk2.value = new MissingMTrk2();
        return MissingMTrk2;
      }();
      VarLenExccededMaxDepth = /* @__PURE__ */ function() {
        function VarLenExccededMaxDepth2() {
        }
        ;
        VarLenExccededMaxDepth2.value = new VarLenExccededMaxDepth2();
        return VarLenExccededMaxDepth2;
      }();
      InvalidHeaderLengthVal = /* @__PURE__ */ function() {
        function InvalidHeaderLengthVal2() {
        }
        ;
        InvalidHeaderLengthVal2.value = new InvalidHeaderLengthVal2();
        return InvalidHeaderLengthVal2;
      }();
      UnspecifiedMidiEvent = /* @__PURE__ */ function() {
        function UnspecifiedMidiEvent2() {
        }
        ;
        UnspecifiedMidiEvent2.value = new UnspecifiedMidiEvent2();
        return UnspecifiedMidiEvent2;
      }();
      UnspecifiedMetaEvent = /* @__PURE__ */ function() {
        function UnspecifiedMetaEvent2() {
        }
        ;
        UnspecifiedMetaEvent2.value = new UnspecifiedMetaEvent2();
        return UnspecifiedMetaEvent2;
      }();
      NoError = /* @__PURE__ */ function() {
        function NoError2() {
        }
        ;
        NoError2.value = new NoError2();
        return NoError2;
      }();
      semigroupParseError = {
        append: function(v) {
          return function(v1) {
            if (v instanceof ParseError && v1 instanceof ParseError) {
              return new ParseError(v.value0 + (" ;" + v1.value0));
            }
            ;
            if (v instanceof NoError) {
              return v1;
            }
            ;
            if (v1 instanceof NoError) {
              return v;
            }
            ;
            return v;
          };
        }
      };
      alt5 = /* @__PURE__ */ alt(/* @__PURE__ */ altStateT(monadExceptT2)(/* @__PURE__ */ altExceptT(semigroupParseError)(monadIdentity)));
      monoidParseError = /* @__PURE__ */ function() {
        return {
          mempty: NoError.value,
          Semigroup0: function() {
            return semigroupParseError;
          }
        };
      }();
      readInt2 = /* @__PURE__ */ bind5(get2)(function(v) {
        var v1 = index(v.file)(v.pos);
        if (v1 instanceof Just) {
          return discard5(put2(function() {
            var $164 = {};
            for (var $165 in v) {
              if ({}.hasOwnProperty.call(v, $165)) {
                $164[$165] = v[$165];
              }
              ;
            }
            ;
            $164.pos = v.pos + 1 | 0;
            return $164;
          }()))(function() {
            return pure9(v1.value0);
          });
        }
        ;
        return lift4(throwError3(new ParseError("failed to read byte!")));
      });
      readManyInts = function(n) {
        return sequence2(replicate(n)(readInt2));
      };
      readInt16 = /* @__PURE__ */ bind5(/* @__PURE__ */ readManyInts(2))(function(bytes) {
        if (bytes.length === 2) {
          return pure9(combine2(bytes[0])(bytes[1]));
        }
        ;
        return lift4(throwError3(new ParseError("failed to read two bits")));
      });
      readInt32 = /* @__PURE__ */ bind5(/* @__PURE__ */ readManyInts(4))(function(bytes) {
        if (bytes.length === 4) {
          return pure9(combine4(bytes[0])(bytes[1])(bytes[2])(bytes[3]));
        }
        ;
        return lift4(throwError3(new ParseError("failed to read four bits")));
      });
      peekInt = /* @__PURE__ */ bind5(get2)(function(v) {
        var v1 = index(v.file)(v.pos);
        if (v1 instanceof Just) {
          return pure9(v1.value0);
        }
        ;
        return lift4(throwError3(new ParseError("failed to peek byte")));
      });
      parseVarLen = /* @__PURE__ */ function() {
        var f = function(z) {
          return function(depth) {
            return bind5(readInt2)(function(val) {
              var val$prime = 127 & val;
              var next = z << 7 | val$prime;
              var mask = 128 & val;
              if (mask === 128) {
                var $194 = depth > 3;
                if ($194) {
                  return throwError1(VarLenExccededMaxDepth.value);
                }
                ;
                return f(next)(depth + 1 | 0);
              }
              ;
              return pure9(next);
            });
          };
        };
        return f(0)(0);
      }();
      parseTextEvent = /* @__PURE__ */ bind5(parseVarLen)(function(len) {
        return bind5(mapFlipped2(readManyInts(len))(mapMaybe(fromCharCode2)))(function(text6) {
          return pure9(fromCharArray(text6));
        });
      });
      parseSeqSpec = /* @__PURE__ */ bind5(parseVarLen)(function() {
        return pure9(SeqSpec.value);
      });
      parseProgChange = /* @__PURE__ */ bind5(/* @__PURE__ */ map19(/* @__PURE__ */ and2(15))(readInt2))(function(chan) {
        return bind5(readInt2)(function(progNum) {
          return pure9(new ProgChange({
            chan,
            progNum
          }));
        });
      });
      parsePitchWheel = /* @__PURE__ */ bind5(/* @__PURE__ */ map19(/* @__PURE__ */ and2(15))(readInt2))(function(chan) {
        return bind5(readInt16)(function(pos) {
          return pure9(new PitchWheel({
            chan,
            pos
          }));
        });
      });
      parseNoteOn = /* @__PURE__ */ bind5(/* @__PURE__ */ map19(/* @__PURE__ */ and2(15))(readInt2))(function(chan) {
        return bind5(readInt2)(function(key) {
          return bind5(readInt2)(function(vel) {
            return pure9(new NoteOn({
              key,
              vel,
              chan
            }));
          });
        });
      });
      parseNoteOff = /* @__PURE__ */ bind5(/* @__PURE__ */ map19(/* @__PURE__ */ and2(15))(readInt2))(function(chan) {
        return bind5(readInt2)(function(key) {
          return bind5(readInt2)(function(vel) {
            return pure9(new NoteOff({
              key,
              vel,
              chan
            }));
          });
        });
      });
      parseHeader = /* @__PURE__ */ bind5(/* @__PURE__ */ readManyInts(4))(function(arr) {
        return discard5(unless2(eq2(arr)([77, 84, 104, 100]))(throwError1(MissingMThd.value)))(function() {
          return bind5(readInt32)(function(lenVal) {
            return discard5(unless2(lenVal === 6)(throwError1(InvalidHeaderLengthVal.value)))(function() {
              return bind5(readInt16)(function(format) {
                return bind5(readInt16)(function(nTracks) {
                  return bind5(readInt16)(function(division) {
                    return pure9({
                      format,
                      nTracks,
                      division
                    });
                  });
                });
              });
            });
          });
        });
      });
      parseCC = /* @__PURE__ */ bind5(/* @__PURE__ */ map19(/* @__PURE__ */ and2(15))(readInt2))(function(chan) {
        return bind5(readInt2)(function(ctrl) {
          return bind5(readInt2)(function(val) {
            return pure9(new CC({
              chan,
              ctrl,
              val
            }));
          });
        });
      });
      parseAfterTouch = /* @__PURE__ */ bind5(/* @__PURE__ */ map19(/* @__PURE__ */ and2(15))(readInt2))(function(chan) {
        return bind5(readInt2)(function(val) {
          return pure9(new AfterTouch({
            chan,
            val
          }));
        });
      });
      parseMidiEvent = /* @__PURE__ */ bind5(/* @__PURE__ */ map19(/* @__PURE__ */ and2(240))(peekInt))(function(leader) {
        if (leader === 128) {
          return bind5(parseNoteOff)(function(noteOff) {
            return pure9(noteOff);
          });
        }
        ;
        if (leader === 144) {
          return bind5(parseNoteOn)(function(noteOn) {
            return pure9(noteOn);
          });
        }
        ;
        if (leader === 176) {
          return bind5(parseCC)(function(cc) {
            return pure9(cc);
          });
        }
        ;
        if (leader === 192) {
          return bind5(parseProgChange)(function(prog) {
            return pure9(prog);
          });
        }
        ;
        if (leader === 208) {
          return bind5(parseAfterTouch)(function(afterTouch) {
            return pure9(afterTouch);
          });
        }
        ;
        if (leader === 224) {
          return bind5(parsePitchWheel)(function(pitchWheel) {
            return pure9(pitchWheel);
          });
        }
        ;
        return throwError1(UnspecifiedMidiEvent.value);
      });
      many = /* @__PURE__ */ manyRec(/* @__PURE__ */ monadRecStateT(/* @__PURE__ */ monadRecExceptT(monadRecIdentity)))(/* @__PURE__ */ alternativeStateT(monadExceptT2)(/* @__PURE__ */ alternativeExceptT(monoidParseError)(monadIdentity)));
      assertNext = function(n) {
        return bind5(readInt2)(function($$byte) {
          return when4($$byte !== n)(throwError1(new ParseError("next byte was not equal to " + show1(n))));
        });
      };
      parseChanPrefix = /* @__PURE__ */ discard5(/* @__PURE__ */ assertNext(1))(function() {
        return bind5(readInt2)(function(prefix) {
          return pure9(new ChannelPrefix(prefix));
        });
      });
      parseEndOfTrack = /* @__PURE__ */ discard5(/* @__PURE__ */ assertNext(0))(function() {
        return pure9(EndOfTrack.value);
      });
      parseKeySig = /* @__PURE__ */ discard5(/* @__PURE__ */ assertNext(2))(function() {
        return bind5(readInt2)(function(sf) {
          return discard5(unless2(sf >= (-1 | 0) && sf <= 7)(throwError1(new ParseError("sharps or flats byte out of valid range"))))(function() {
            return bind5(readInt2)(function(mi) {
              return discard5(unless2(mi === 1 || mi === 0)(throwError1(new ParseError("key byte not 1 or 0"))))(function() {
                return pure9(new KeySigEv({
                  sf,
                  mi
                }));
              });
            });
          });
        });
      });
      parseSMTPEOffset = /* @__PURE__ */ discard5(/* @__PURE__ */ assertNext(5))(function() {
        return bind5(readInt2)(function(hr2) {
          return bind5(readInt2)(function(mn) {
            return bind5(readInt2)(function(sec) {
              return bind5(readInt2)(function(fr) {
                return bind5(readInt2)(function(fFr) {
                  return pure9(new SmpteOffset({
                    hr: hr2,
                    mn,
                    sec,
                    fr,
                    fFr
                  }));
                });
              });
            });
          });
        });
      });
      parseSeqNum = /* @__PURE__ */ discard5(/* @__PURE__ */ assertNext(2))(function() {
        return bind5(readInt16)(function(num) {
          return pure9(new SeqNum(num));
        });
      });
      parseTempo = /* @__PURE__ */ discard5(/* @__PURE__ */ assertNext(3))(function() {
        return bind5(readInt2)(function(a2) {
          return bind5(readInt2)(function(b2) {
            return bind5(readInt2)(function(c) {
              return pure9(new Tempo(combine3(a2)(b2)(c)));
            });
          });
        });
      });
      parseTimeSig = /* @__PURE__ */ discard5(/* @__PURE__ */ assertNext(4))(function() {
        return bind5(readInt2)(function(nn) {
          return bind5(readInt2)(function(dd2) {
            return bind5(readInt2)(function(cc) {
              return bind5(readInt2)(function(bb) {
                var denom = fromNumber(1 / pow(toNumber(dd2))(-2));
                if (denom instanceof Just) {
                  return pure9(new TimeSigEv({
                    nn,
                    dd: denom.value0,
                    cc,
                    bb
                  }));
                }
                ;
                return throwError1(new ParseError("couldn't convert timesig denom"));
              });
            });
          });
        });
      });
      parseMetaEvent = /* @__PURE__ */ discard5(/* @__PURE__ */ assertNext(255))(function() {
        return bind5(readInt2)(function(leader) {
          if (leader === 0) {
            return bind5(parseSeqNum)(function(seqNum) {
              return pure9(seqNum);
            });
          }
          ;
          if (leader === 1) {
            return bind5(parseTextEvent)(function(text6) {
              return pure9(new Text2(text6));
            });
          }
          ;
          if (leader === 2) {
            return bind5(parseTextEvent)(function(text6) {
              return pure9(new Copyright(text6));
            });
          }
          ;
          if (leader === 3) {
            return bind5(parseTextEvent)(function(text6) {
              return pure9(new TrackName(text6));
            });
          }
          ;
          if (leader === 4) {
            return bind5(parseTextEvent)(function(text6) {
              return pure9(new InstName(text6));
            });
          }
          ;
          if (leader === 5) {
            return bind5(parseTextEvent)(function(text6) {
              return pure9(new Lyric(text6));
            });
          }
          ;
          if (leader === 6) {
            return bind5(parseTextEvent)(function(text6) {
              return pure9(new Marker(text6));
            });
          }
          ;
          if (leader === 7) {
            return bind5(parseTextEvent)(function(text6) {
              return pure9(new CuePoint(text6));
            });
          }
          ;
          if (leader === 32) {
            return bind5(parseChanPrefix)(function(chanPrefix) {
              return pure9(chanPrefix);
            });
          }
          ;
          if (leader === 47) {
            return bind5(parseEndOfTrack)(function(eot) {
              return pure9(eot);
            });
          }
          ;
          if (leader === 81) {
            return bind5(parseTempo)(function(tempo) {
              return pure9(tempo);
            });
          }
          ;
          if (leader === 84) {
            return bind5(parseSMTPEOffset)(function(offset) {
              return pure9(offset);
            });
          }
          ;
          if (leader === 88) {
            return bind5(parseTimeSig)(function(sig) {
              return pure9(sig);
            });
          }
          ;
          if (leader === 89) {
            return bind5(parseKeySig)(function(sig) {
              return pure9(sig);
            });
          }
          ;
          if (leader === 127) {
            return bind5(parseSeqSpec)(function(spec) {
              return pure9(spec);
            });
          }
          ;
          return throwError1(UnspecifiedMetaEvent.value);
        });
      });
      parseTrackEvent = /* @__PURE__ */ bind5(parseVarLen)(function(delta) {
        return bind5(alt5(map19(flip(MetaEvent.create)(delta))(parseMetaEvent))(map19(flip(MidiEvent.create)(delta))(parseMidiEvent)))(function(event) {
          return pure9(event);
        });
      });
      parseTrack = /* @__PURE__ */ bind5(/* @__PURE__ */ readManyInts(4))(function(mtrk) {
        return discard5(unless2(eq2(mtrk)([77, 84, 114, 107]))(throwError1(MissingMTrk.value)))(function() {
          return bind5(readInt32)(function() {
            return bind5(map19(fromFoldable3)(many(parseTrackEvent)))(function(events) {
              return pure9({
                events
              });
            });
          });
        });
      });
      parseFile = /* @__PURE__ */ bind5(parseHeader)(function(header2) {
        return bind5(map19(fromFoldable3)(many(parseTrack)))(function(tracks) {
          return pure9({
            header: header2,
            tracks
          });
        });
      });
    }
  });

  // output/Main/index.js
  var Main_exports = {};
  __export(Main_exports, {
    GodSpeed: () => GodSpeed,
    component: () => component,
    foo: () => foo,
    handleAction: () => handleAction,
    initialState: () => initialState,
    main: () => main2,
    readFileFromFilePickEvent: () => readFileFromFilePickEvent,
    render: () => render
  });
  var bind6, unwrap4, evalStateT2, pure10, modify_3, show2, showRecord3, show12, type_19, GodSpeed, initialState, handleAction, foo, render, component, component1, main2;
  var init_Main = __esm({
    "output/Main/index.js"() {
      init_foreign();
      init_Control4();
      init_Control5();
      init_Control_Monad_Except();
      init_Control_Monad_State();
      init_Control_Monad_State2();
      init_DOM_HTML_Indexed();
      init_DOM_HTML_Indexed2();
      init_Data32();
      init_Data16();
      init_Data20();
      init_Data15();
      init_Data25();
      init_Data14();
      init_Data3();
      init_Effect6();
      init_Effect_Aff();
      init_Effect_Aff2();
      init_Halogen_Aff();
      init_Halogen3();
      init_Halogen_HTML();
      init_Halogen_HTML2();
      init_Halogen_HTML3();
      init_Halogen_HTML4();
      init_Halogen_Query3();
      init_Halogen_VDom6();
      init_MidiTypes();
      init_StateParser();
      init_foreign();
      bind6 = /* @__PURE__ */ bind(bindHalogenM);
      unwrap4 = /* @__PURE__ */ unwrap();
      evalStateT2 = /* @__PURE__ */ evalStateT(/* @__PURE__ */ functorExceptT(functorIdentity));
      pure10 = /* @__PURE__ */ pure(applicativeHalogenM);
      modify_3 = /* @__PURE__ */ modify_2(monadStateHalogenM);
      show2 = /* @__PURE__ */ show(showInt);
      showRecord3 = /* @__PURE__ */ showRecord()();
      show12 = /* @__PURE__ */ show(/* @__PURE__ */ showRecord3(/* @__PURE__ */ showRecordFieldsCons({
        reflectSymbol: function() {
          return "header";
        }
      })(/* @__PURE__ */ showRecordFieldsConsNil({
        reflectSymbol: function() {
          return "tracks";
        }
      })(/* @__PURE__ */ showArray(/* @__PURE__ */ showRecord3(/* @__PURE__ */ showRecordFieldsConsNil({
        reflectSymbol: function() {
          return "events";
        }
      })(/* @__PURE__ */ showArray(showEvent))))))(/* @__PURE__ */ showRecord3(/* @__PURE__ */ showRecordFieldsCons({
        reflectSymbol: function() {
          return "division";
        }
      })(/* @__PURE__ */ showRecordFieldsCons({
        reflectSymbol: function() {
          return "format";
        }
      })(/* @__PURE__ */ showRecordFieldsConsNil({
        reflectSymbol: function() {
          return "nTracks";
        }
      })(showInt))(showInt))(showInt)))));
      type_19 = /* @__PURE__ */ type_18(isPropInputType);
      GodSpeed = /* @__PURE__ */ function() {
        function GodSpeed2(value0) {
          this.value0 = value0;
        }
        ;
        GodSpeed2.create = function(value0) {
          return new GodSpeed2(value0);
        };
        return GodSpeed2;
      }();
      initialState = function(v) {
        return {
          mMidiFile: Nothing.value
        };
      };
      handleAction = function(dictMonadAff) {
        var liftAff2 = liftAff(monadAffHalogenM(dictMonadAff));
        return function(v) {
          return bind6(liftAff2(fromEffectFnAff(readFileFromFilePickEvent({
            just: Just.create,
            nothing: Nothing.value,
            event: v.value0
          }))))(function(mFile) {
            var parsed = unwrap4(runExceptT(evalStateT2(parseFile)({
              file: fromMaybe([])(mFile),
              pos: 0
            })));
            if (parsed instanceof Left) {
              return pure10(unit);
            }
            ;
            if (parsed instanceof Right) {
              return modify_3(function(st) {
                var $51 = {};
                for (var $52 in st) {
                  if ({}.hasOwnProperty.call(st, $52)) {
                    $51[$52] = st[$52];
                  }
                  ;
                }
                ;
                $51.mMidiFile = new Just(parsed.value0);
                return $51;
              });
            }
            ;
            throw new Error("Failed pattern match at Main (line 90, column 9 - line 93, column 64): " + [parsed.constructor.name]);
          });
        };
      };
      foo = function(st) {
        if (st.mMidiFile instanceof Nothing) {
          return p_([text5("")]);
        }
        ;
        if (st.mMidiFile instanceof Just) {
          return div_([p_([text5(show2(length(st.mMidiFile.value0.tracks)))]), p_([text5(show12(st.mMidiFile.value0))])]);
        }
        ;
        throw new Error("Failed pattern match at Main (line 68, column 5 - line 73, column 14): " + [st.mMidiFile.constructor.name]);
      };
      render = function(state3) {
        return div_([h1_([text5("QuickClean foo")]), foo(state3), form_([input([type_19(InputFile.value), accept2([new AcceptFileExtension(".mid")]), onChange(function(event) {
          return new GodSpeed(event);
        })])])]);
      };
      component = function(dictMonadAff) {
        return mkComponent({
          initialState,
          render,
          "eval": mkEval({
            handleQuery: defaultEval.handleQuery,
            receive: defaultEval.receive,
            initialize: defaultEval.initialize,
            finalize: defaultEval.finalize,
            handleAction: handleAction(dictMonadAff)
          })
        });
      };
      component1 = /* @__PURE__ */ component(monadAffAff);
      main2 = /* @__PURE__ */ runHalogenAff(/* @__PURE__ */ bind(bindAff)(awaitBody)(function(body2) {
        return runUI2(component1)(unit)(body2);
      }));
    }
  });

  // index.js
  (init_Main(), __toCommonJS(Main_exports)).main();
})();
