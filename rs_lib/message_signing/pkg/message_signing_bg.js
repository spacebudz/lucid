let wasm;
export function __wbg_set_wasm(val) {
    wasm = val;
}


const heap = new Array(128).fill(undefined);

heap.push(undefined, null, true, false);

function getObject(idx) { return heap[idx]; }

function debugString(val) {
    // primitive types
    const type = typeof val;
    if (type == 'number' || type == 'boolean' || val == null) {
        return  `${val}`;
    }
    if (type == 'string') {
        return `"${val}"`;
    }
    if (type == 'symbol') {
        const description = val.description;
        if (description == null) {
            return 'Symbol';
        } else {
            return `Symbol(${description})`;
        }
    }
    if (type == 'function') {
        const name = val.name;
        if (typeof name == 'string' && name.length > 0) {
            return `Function(${name})`;
        } else {
            return 'Function';
        }
    }
    // objects
    if (Array.isArray(val)) {
        const length = val.length;
        let debug = '[';
        if (length > 0) {
            debug += debugString(val[0]);
        }
        for(let i = 1; i < length; i++) {
            debug += ', ' + debugString(val[i]);
        }
        debug += ']';
        return debug;
    }
    // Test for built-in
    const builtInMatches = /\[object ([^\]]+)\]/.exec(toString.call(val));
    let className;
    if (builtInMatches && builtInMatches.length > 1) {
        className = builtInMatches[1];
    } else {
        // Failed to match the standard '[object ClassName]'
        return toString.call(val);
    }
    if (className == 'Object') {
        // we're a user defined class or Object
        // JSON.stringify avoids problems with cycles, and is generally much
        // easier than looping through ownProperties of `val`.
        try {
            return 'Object(' + JSON.stringify(val) + ')';
        } catch (_) {
            return 'Object';
        }
    }
    // errors
    if (val instanceof Error) {
        return `${val.name}: ${val.message}\n${val.stack}`;
    }
    // TODO we could test for more things here, like `Set`s and `Map`s.
    return className;
}

let WASM_VECTOR_LEN = 0;

let cachedUint8ArrayMemory0 = null;

function getUint8ArrayMemory0() {
    if (cachedUint8ArrayMemory0 === null || cachedUint8ArrayMemory0.byteLength === 0) {
        cachedUint8ArrayMemory0 = new Uint8Array(wasm.memory.buffer);
    }
    return cachedUint8ArrayMemory0;
}

const lTextEncoder = typeof TextEncoder === 'undefined' ? (0, module.require)('util').TextEncoder : TextEncoder;

let cachedTextEncoder = new lTextEncoder('utf-8');

const encodeString = (typeof cachedTextEncoder.encodeInto === 'function'
    ? function (arg, view) {
    return cachedTextEncoder.encodeInto(arg, view);
}
    : function (arg, view) {
    const buf = cachedTextEncoder.encode(arg);
    view.set(buf);
    return {
        read: arg.length,
        written: buf.length
    };
});

function passStringToWasm0(arg, malloc, realloc) {

    if (realloc === undefined) {
        const buf = cachedTextEncoder.encode(arg);
        const ptr = malloc(buf.length, 1) >>> 0;
        getUint8ArrayMemory0().subarray(ptr, ptr + buf.length).set(buf);
        WASM_VECTOR_LEN = buf.length;
        return ptr;
    }

    let len = arg.length;
    let ptr = malloc(len, 1) >>> 0;

    const mem = getUint8ArrayMemory0();

    let offset = 0;

    for (; offset < len; offset++) {
        const code = arg.charCodeAt(offset);
        if (code > 0x7F) break;
        mem[ptr + offset] = code;
    }

    if (offset !== len) {
        if (offset !== 0) {
            arg = arg.slice(offset);
        }
        ptr = realloc(ptr, len, len = offset + arg.length * 3, 1) >>> 0;
        const view = getUint8ArrayMemory0().subarray(ptr + offset, ptr + len);
        const ret = encodeString(arg, view);

        offset += ret.written;
        ptr = realloc(ptr, len, offset, 1) >>> 0;
    }

    WASM_VECTOR_LEN = offset;
    return ptr;
}

let cachedDataViewMemory0 = null;

function getDataViewMemory0() {
    if (cachedDataViewMemory0 === null || cachedDataViewMemory0.buffer.detached === true || (cachedDataViewMemory0.buffer.detached === undefined && cachedDataViewMemory0.buffer !== wasm.memory.buffer)) {
        cachedDataViewMemory0 = new DataView(wasm.memory.buffer);
    }
    return cachedDataViewMemory0;
}

let heap_next = heap.length;

function dropObject(idx) {
    if (idx < 132) return;
    heap[idx] = heap_next;
    heap_next = idx;
}

function takeObject(idx) {
    const ret = getObject(idx);
    dropObject(idx);
    return ret;
}

const lTextDecoder = typeof TextDecoder === 'undefined' ? (0, module.require)('util').TextDecoder : TextDecoder;

let cachedTextDecoder = new lTextDecoder('utf-8', { ignoreBOM: true, fatal: true });

cachedTextDecoder.decode();

function getStringFromWasm0(ptr, len) {
    ptr = ptr >>> 0;
    return cachedTextDecoder.decode(getUint8ArrayMemory0().subarray(ptr, ptr + len));
}

function addHeapObject(obj) {
    if (heap_next === heap.length) heap.push(heap.length + 1);
    const idx = heap_next;
    heap_next = heap[idx];

    heap[idx] = obj;
    return idx;
}

function _assertClass(instance, klass) {
    if (!(instance instanceof klass)) {
        throw new Error(`expected instance of ${klass.name}`);
    }
}

function passArray8ToWasm0(arg, malloc) {
    const ptr = malloc(arg.length * 1, 1) >>> 0;
    getUint8ArrayMemory0().set(arg, ptr / 1);
    WASM_VECTOR_LEN = arg.length;
    return ptr;
}

function getArrayU8FromWasm0(ptr, len) {
    ptr = ptr >>> 0;
    return getUint8ArrayMemory0().subarray(ptr / 1, ptr / 1 + len);
}

function isLikeNone(x) {
    return x === undefined || x === null;
}
/**
 * @enum {0 | 1}
 */
export const AlgorithmId = Object.freeze({
    /**
     * r" EdDSA (Pure EdDSA, not HashedEdDSA) - the algorithm used for Cardano addresses
     */
    EdDSA: 0, "0": "EdDSA",
    /**
     * r" ChaCha20/Poly1305 w/ 256-bit key, 128-bit tag
     */
    ChaCha20Poly1305: 1, "1": "ChaCha20Poly1305",
});
/**
 * @enum {0 | 1 | 2 | 3 | 4 | 5}
 */
export const CBORSpecialType = Object.freeze({
    Bool: 0, "0": "Bool",
    Float: 1, "1": "Float",
    Unassigned: 2, "2": "Unassigned",
    Break: 3, "3": "Break",
    Undefined: 4, "4": "Undefined",
    Null: 5, "5": "Null",
});
/**
 * @enum {0 | 1 | 2 | 3 | 4 | 5 | 6}
 */
export const CBORValueKind = Object.freeze({
    Int: 0, "0": "Int",
    Bytes: 1, "1": "Bytes",
    Text: 2, "2": "Text",
    Array: 3, "3": "Array",
    Object: 4, "4": "Object",
    TaggedCBOR: 5, "5": "TaggedCBOR",
    Special: 6, "6": "Special",
});
/**
 * @enum {0 | 1 | 2 | 3 | 4 | 5 | 6}
 */
export const CurveType = Object.freeze({
    P256: 0, "0": "P256",
    P384: 1, "1": "P384",
    P521: 2, "2": "P521",
    X25519: 3, "3": "X25519",
    X448: 4, "4": "X448",
    Ed25519: 5, "5": "Ed25519",
    Ed448: 6, "6": "Ed448",
});
/**
 * @enum {0 | 1 | 2 | 3}
 */
export const ECKey = Object.freeze({
    CRV: 0, "0": "CRV",
    X: 1, "1": "X",
    Y: 2, "2": "Y",
    D: 3, "3": "D",
});
/**
 * @enum {0 | 1 | 2 | 3 | 4 | 5 | 6 | 7}
 */
export const KeyOperation = Object.freeze({
    Sign: 0, "0": "Sign",
    Verify: 1, "1": "Verify",
    Encrypt: 2, "2": "Encrypt",
    Decrypt: 3, "3": "Decrypt",
    WrapKey: 4, "4": "WrapKey",
    UnwrapKey: 5, "5": "UnwrapKey",
    DeriveKey: 6, "6": "DeriveKey",
    DeriveBits: 7, "7": "DeriveBits",
});
/**
 * @enum {0 | 1 | 2}
 */
export const KeyType = Object.freeze({
    /**
     * r" octet key pair
     */
    OKP: 0, "0": "OKP",
    /**
     * r" 2-coord EC
     */
    EC2: 1, "1": "EC2",
    Symmetric: 2, "2": "Symmetric",
});
/**
 * @enum {0 | 1}
 */
export const LabelKind = Object.freeze({
    Int: 0, "0": "Int",
    Text: 1, "1": "Text",
});
/**
 * @enum {0 | 1 | 2}
 */
export const SigContext = Object.freeze({
    Signature: 0, "0": "Signature",
    Signature1: 1, "1": "Signature1",
    CounterSignature: 2, "2": "CounterSignature",
});
/**
 * @enum {0 | 1}
 */
export const SignedMessageKind = Object.freeze({
    COSESIGN: 0, "0": "COSESIGN",
    COSESIGN1: 1, "1": "COSESIGN1",
});

const BigNumFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_bignum_free(ptr >>> 0, 1));

export class BigNum {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(BigNum.prototype);
        obj.__wbg_ptr = ptr;
        BigNumFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        BigNumFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_bignum_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.bignum_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {BigNum}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.bignum_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return BigNum.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {string} string
     * @returns {BigNum}
     */
    static from_str(string) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passStringToWasm0(string, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.bignum_from_str(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return BigNum.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {string}
     */
    to_str() {
        let deferred1_0;
        let deferred1_1;
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.bignum_to_str(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            deferred1_0 = r0;
            deferred1_1 = r1;
            return getStringFromWasm0(r0, r1);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
    /**
     * @param {BigNum} other
     * @returns {BigNum}
     */
    checked_mul(other) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            _assertClass(other, BigNum);
            wasm.bignum_checked_mul(retptr, this.__wbg_ptr, other.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return BigNum.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {BigNum} other
     * @returns {BigNum}
     */
    checked_add(other) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            _assertClass(other, BigNum);
            wasm.bignum_checked_add(retptr, this.__wbg_ptr, other.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return BigNum.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {BigNum} other
     * @returns {BigNum}
     */
    checked_sub(other) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            _assertClass(other, BigNum);
            wasm.bignum_checked_sub(retptr, this.__wbg_ptr, other.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return BigNum.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
}

const CBORArrayFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cborarray_free(ptr >>> 0, 1));

export class CBORArray {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(CBORArray.prototype);
        obj.__wbg_ptr = ptr;
        CBORArrayFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        CBORArrayFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_cborarray_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cborarray_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {CBORArray}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.cborarray_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return CBORArray.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {CBORArray}
     */
    static new() {
        const ret = wasm.cborarray_new();
        return CBORArray.__wrap(ret);
    }
    /**
     * @returns {number}
     */
    len() {
        const ret = wasm.cborarray_len(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
     * @param {number} index
     * @returns {CBORValue}
     */
    get(index) {
        const ret = wasm.cborarray_get(this.__wbg_ptr, index);
        return CBORValue.__wrap(ret);
    }
    /**
     * @param {CBORValue} elem
     */
    add(elem) {
        _assertClass(elem, CBORValue);
        wasm.cborarray_add(this.__wbg_ptr, elem.__wbg_ptr);
    }
    /**
     * @param {boolean} use_definite
     */
    set_definite_encoding(use_definite) {
        wasm.cborarray_set_definite_encoding(this.__wbg_ptr, use_definite);
    }
    /**
     * @returns {boolean}
     */
    is_definite() {
        const ret = wasm.cborarray_is_definite(this.__wbg_ptr);
        return ret !== 0;
    }
}

const CBORObjectFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cborobject_free(ptr >>> 0, 1));

export class CBORObject {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(CBORObject.prototype);
        obj.__wbg_ptr = ptr;
        CBORObjectFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        CBORObjectFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_cborobject_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cborobject_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {CBORObject}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.cborobject_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return CBORObject.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {CBORObject}
     */
    static new() {
        const ret = wasm.cborobject_new();
        return CBORObject.__wrap(ret);
    }
    /**
     * @returns {number}
     */
    len() {
        const ret = wasm.cborobject_len(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
     * @param {CBORValue} key
     * @param {CBORValue} value
     * @returns {CBORValue | undefined}
     */
    insert(key, value) {
        _assertClass(key, CBORValue);
        _assertClass(value, CBORValue);
        const ret = wasm.cborobject_insert(this.__wbg_ptr, key.__wbg_ptr, value.__wbg_ptr);
        return ret === 0 ? undefined : CBORValue.__wrap(ret);
    }
    /**
     * @param {CBORValue} key
     * @returns {CBORValue | undefined}
     */
    get(key) {
        _assertClass(key, CBORValue);
        const ret = wasm.cborobject_get(this.__wbg_ptr, key.__wbg_ptr);
        return ret === 0 ? undefined : CBORValue.__wrap(ret);
    }
    /**
     * @returns {CBORArray}
     */
    keys() {
        const ret = wasm.cborobject_keys(this.__wbg_ptr);
        return CBORArray.__wrap(ret);
    }
    /**
     * @param {boolean} use_definite
     */
    set_definite_encoding(use_definite) {
        wasm.cborobject_set_definite_encoding(this.__wbg_ptr, use_definite);
    }
    /**
     * @returns {boolean}
     */
    is_definite() {
        const ret = wasm.cborobject_is_definite(this.__wbg_ptr);
        return ret !== 0;
    }
}

const CBORSpecialFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cborspecial_free(ptr >>> 0, 1));

export class CBORSpecial {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(CBORSpecial.prototype);
        obj.__wbg_ptr = ptr;
        CBORSpecialFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        CBORSpecialFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_cborspecial_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cborspecial_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {CBORSpecial}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.cborspecial_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return CBORSpecial.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {boolean} b
     * @returns {CBORSpecial}
     */
    static new_bool(b) {
        const ret = wasm.cborspecial_new_bool(b);
        return CBORSpecial.__wrap(ret);
    }
    /**
     * @param {number} u
     * @returns {CBORSpecial}
     */
    static new_unassigned(u) {
        const ret = wasm.cborspecial_new_unassigned(u);
        return CBORSpecial.__wrap(ret);
    }
    /**
     * @returns {CBORSpecial}
     */
    static new_break() {
        const ret = wasm.cborspecial_new_break();
        return CBORSpecial.__wrap(ret);
    }
    /**
     * @returns {CBORSpecial}
     */
    static new_null() {
        const ret = wasm.cborspecial_new_null();
        return CBORSpecial.__wrap(ret);
    }
    /**
     * @returns {CBORSpecial}
     */
    static new_undefined() {
        const ret = wasm.cborspecial_new_undefined();
        return CBORSpecial.__wrap(ret);
    }
    /**
     * @returns {CBORSpecialType}
     */
    kind() {
        const ret = wasm.cborspecial_kind(this.__wbg_ptr);
        return ret;
    }
    /**
     * @returns {boolean | undefined}
     */
    as_bool() {
        const ret = wasm.cborspecial_as_bool(this.__wbg_ptr);
        return ret === 0xFFFFFF ? undefined : ret !== 0;
    }
    /**
     * @returns {number | undefined}
     */
    as_float() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cborspecial_as_float(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r2 = getDataViewMemory0().getFloat64(retptr + 8 * 1, true);
            return r0 === 0 ? undefined : r2;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {number | undefined}
     */
    as_unassigned() {
        const ret = wasm.cborspecial_as_unassigned(this.__wbg_ptr);
        return ret === 0xFFFFFF ? undefined : ret;
    }
}

const CBORValueFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cborvalue_free(ptr >>> 0, 1));

export class CBORValue {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(CBORValue.prototype);
        obj.__wbg_ptr = ptr;
        CBORValueFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        CBORValueFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_cborvalue_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cborvalue_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {CBORValue}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.cborvalue_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return CBORValue.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Int} int
     * @returns {CBORValue}
     */
    static new_int(int) {
        _assertClass(int, Int);
        const ret = wasm.cborvalue_new_int(int.__wbg_ptr);
        return CBORValue.__wrap(ret);
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {CBORValue}
     */
    static new_bytes(bytes) {
        const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.cborvalue_new_bytes(ptr0, len0);
        return CBORValue.__wrap(ret);
    }
    /**
     * @param {string} text
     * @returns {CBORValue}
     */
    static new_text(text) {
        const ptr0 = passStringToWasm0(text, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.cborvalue_new_text(ptr0, len0);
        return CBORValue.__wrap(ret);
    }
    /**
     * @param {CBORArray} arr
     * @returns {CBORValue}
     */
    static new_array(arr) {
        _assertClass(arr, CBORArray);
        const ret = wasm.cborvalue_new_array(arr.__wbg_ptr);
        return CBORValue.__wrap(ret);
    }
    /**
     * @param {CBORObject} obj
     * @returns {CBORValue}
     */
    static new_object(obj) {
        _assertClass(obj, CBORObject);
        const ret = wasm.cborvalue_new_object(obj.__wbg_ptr);
        return CBORValue.__wrap(ret);
    }
    /**
     * @param {TaggedCBOR} tagged
     * @returns {CBORValue}
     */
    static new_tagged(tagged) {
        _assertClass(tagged, TaggedCBOR);
        const ret = wasm.cborvalue_new_tagged(tagged.__wbg_ptr);
        return CBORValue.__wrap(ret);
    }
    /**
     * @param {CBORSpecial} special
     * @returns {CBORValue}
     */
    static new_special(special) {
        _assertClass(special, CBORSpecial);
        const ret = wasm.cborvalue_new_special(special.__wbg_ptr);
        return CBORValue.__wrap(ret);
    }
    /**
     * @param {Label} label
     * @returns {CBORValue}
     */
    static from_label(label) {
        _assertClass(label, Label);
        const ret = wasm.cborvalue_from_label(label.__wbg_ptr);
        return CBORValue.__wrap(ret);
    }
    /**
     * @returns {CBORValueKind}
     */
    kind() {
        const ret = wasm.cborvalue_kind(this.__wbg_ptr);
        return ret;
    }
    /**
     * @returns {Int | undefined}
     */
    as_int() {
        const ret = wasm.cborvalue_as_int(this.__wbg_ptr);
        return ret === 0 ? undefined : Int.__wrap(ret);
    }
    /**
     * @returns {Uint8Array | undefined}
     */
    as_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cborvalue_as_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            let v1;
            if (r0 !== 0) {
                v1 = getArrayU8FromWasm0(r0, r1).slice();
                wasm.__wbindgen_free(r0, r1 * 1, 1);
            }
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {string | undefined}
     */
    as_text() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cborvalue_as_text(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            let v1;
            if (r0 !== 0) {
                v1 = getStringFromWasm0(r0, r1).slice();
                wasm.__wbindgen_free(r0, r1 * 1, 1);
            }
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {CBORArray | undefined}
     */
    as_array() {
        const ret = wasm.cborvalue_as_array(this.__wbg_ptr);
        return ret === 0 ? undefined : CBORArray.__wrap(ret);
    }
    /**
     * @returns {CBORObject | undefined}
     */
    as_object() {
        const ret = wasm.cborvalue_as_object(this.__wbg_ptr);
        return ret === 0 ? undefined : CBORObject.__wrap(ret);
    }
    /**
     * @returns {TaggedCBOR | undefined}
     */
    as_tagged() {
        const ret = wasm.cborvalue_as_tagged(this.__wbg_ptr);
        return ret === 0 ? undefined : TaggedCBOR.__wrap(ret);
    }
    /**
     * @returns {CBORSpecial | undefined}
     */
    as_special() {
        const ret = wasm.cborvalue_as_special(this.__wbg_ptr);
        return ret === 0 ? undefined : CBORSpecial.__wrap(ret);
    }
}

const COSEEncryptFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_coseencrypt_free(ptr >>> 0, 1));

export class COSEEncrypt {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(COSEEncrypt.prototype);
        obj.__wbg_ptr = ptr;
        COSEEncryptFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        COSEEncryptFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_coseencrypt_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.coseencrypt_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {COSEEncrypt}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.coseencrypt_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return COSEEncrypt.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {Headers}
     */
    headers() {
        const ret = wasm.coseencrypt_headers(this.__wbg_ptr);
        return Headers.__wrap(ret);
    }
    /**
     * @returns {Uint8Array | undefined}
     */
    ciphertext() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.coseencrypt_ciphertext(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            let v1;
            if (r0 !== 0) {
                v1 = getArrayU8FromWasm0(r0, r1).slice();
                wasm.__wbindgen_free(r0, r1 * 1, 1);
            }
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {COSERecipients}
     */
    recipients() {
        const ret = wasm.coseencrypt_recipients(this.__wbg_ptr);
        return COSERecipients.__wrap(ret);
    }
    /**
     * @param {Headers} headers
     * @param {Uint8Array | null | undefined} ciphertext
     * @param {COSERecipients} recipients
     * @returns {COSEEncrypt}
     */
    static new(headers, ciphertext, recipients) {
        _assertClass(headers, Headers);
        var ptr0 = isLikeNone(ciphertext) ? 0 : passArray8ToWasm0(ciphertext, wasm.__wbindgen_malloc);
        var len0 = WASM_VECTOR_LEN;
        _assertClass(recipients, COSERecipients);
        const ret = wasm.coseencrypt_new(headers.__wbg_ptr, ptr0, len0, recipients.__wbg_ptr);
        return COSEEncrypt.__wrap(ret);
    }
}

const COSEEncrypt0Finalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_coseencrypt0_free(ptr >>> 0, 1));

export class COSEEncrypt0 {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(COSEEncrypt0.prototype);
        obj.__wbg_ptr = ptr;
        COSEEncrypt0Finalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        COSEEncrypt0Finalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_coseencrypt0_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.coseencrypt0_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {COSEEncrypt0}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.coseencrypt0_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return COSEEncrypt0.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {Headers}
     */
    headers() {
        const ret = wasm.coseencrypt0_headers(this.__wbg_ptr);
        return Headers.__wrap(ret);
    }
    /**
     * @returns {Uint8Array | undefined}
     */
    ciphertext() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.coseencrypt0_ciphertext(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            let v1;
            if (r0 !== 0) {
                v1 = getArrayU8FromWasm0(r0, r1).slice();
                wasm.__wbindgen_free(r0, r1 * 1, 1);
            }
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Headers} headers
     * @param {Uint8Array | null} [ciphertext]
     * @returns {COSEEncrypt0}
     */
    static new(headers, ciphertext) {
        _assertClass(headers, Headers);
        var ptr0 = isLikeNone(ciphertext) ? 0 : passArray8ToWasm0(ciphertext, wasm.__wbindgen_malloc);
        var len0 = WASM_VECTOR_LEN;
        const ret = wasm.coseencrypt0_new(headers.__wbg_ptr, ptr0, len0);
        return COSEEncrypt0.__wrap(ret);
    }
}

const COSEKeyFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cosekey_free(ptr >>> 0, 1));

export class COSEKey {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(COSEKey.prototype);
        obj.__wbg_ptr = ptr;
        COSEKeyFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        COSEKeyFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_cosekey_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cosekey_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {COSEKey}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.cosekey_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return COSEKey.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Label} key_type
     */
    set_key_type(key_type) {
        _assertClass(key_type, Label);
        wasm.cosekey_set_key_type(this.__wbg_ptr, key_type.__wbg_ptr);
    }
    /**
     * @returns {Label}
     */
    key_type() {
        const ret = wasm.cosekey_key_type(this.__wbg_ptr);
        return Label.__wrap(ret);
    }
    /**
     * @param {Uint8Array} key_id
     */
    set_key_id(key_id) {
        const ptr0 = passArray8ToWasm0(key_id, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.cosekey_set_key_id(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * @returns {Uint8Array | undefined}
     */
    key_id() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cosekey_key_id(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            let v1;
            if (r0 !== 0) {
                v1 = getArrayU8FromWasm0(r0, r1).slice();
                wasm.__wbindgen_free(r0, r1 * 1, 1);
            }
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Label} algorithm_id
     */
    set_algorithm_id(algorithm_id) {
        _assertClass(algorithm_id, Label);
        wasm.cosekey_set_algorithm_id(this.__wbg_ptr, algorithm_id.__wbg_ptr);
    }
    /**
     * @returns {Label | undefined}
     */
    algorithm_id() {
        const ret = wasm.cosekey_algorithm_id(this.__wbg_ptr);
        return ret === 0 ? undefined : Label.__wrap(ret);
    }
    /**
     * @param {Labels} key_ops
     */
    set_key_ops(key_ops) {
        _assertClass(key_ops, Labels);
        wasm.cosekey_set_key_ops(this.__wbg_ptr, key_ops.__wbg_ptr);
    }
    /**
     * @returns {Labels | undefined}
     */
    key_ops() {
        const ret = wasm.cosekey_key_ops(this.__wbg_ptr);
        return ret === 0 ? undefined : Labels.__wrap(ret);
    }
    /**
     * @param {Uint8Array} base_init_vector
     */
    set_base_init_vector(base_init_vector) {
        const ptr0 = passArray8ToWasm0(base_init_vector, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.cosekey_set_base_init_vector(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * @returns {Uint8Array | undefined}
     */
    base_init_vector() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cosekey_base_init_vector(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            let v1;
            if (r0 !== 0) {
                v1 = getArrayU8FromWasm0(r0, r1).slice();
                wasm.__wbindgen_free(r0, r1 * 1, 1);
            }
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Label} label
     * @returns {CBORValue | undefined}
     */
    header(label) {
        _assertClass(label, Label);
        const ret = wasm.cosekey_header(this.__wbg_ptr, label.__wbg_ptr);
        return ret === 0 ? undefined : CBORValue.__wrap(ret);
    }
    /**
     * @param {Label} label
     * @param {CBORValue} value
     */
    set_header(label, value) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            _assertClass(label, Label);
            _assertClass(value, CBORValue);
            wasm.cosekey_set_header(retptr, this.__wbg_ptr, label.__wbg_ptr, value.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            if (r1) {
                throw takeObject(r0);
            }
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Label} key_type
     * @returns {COSEKey}
     */
    static new(key_type) {
        _assertClass(key_type, Label);
        const ret = wasm.cosekey_new(key_type.__wbg_ptr);
        return COSEKey.__wrap(ret);
    }
}

const COSERecipientFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_coserecipient_free(ptr >>> 0, 1));

export class COSERecipient {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(COSERecipient.prototype);
        obj.__wbg_ptr = ptr;
        COSERecipientFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        COSERecipientFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_coserecipient_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.coserecipient_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {COSERecipient}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.coserecipient_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return COSERecipient.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {Headers}
     */
    headers() {
        const ret = wasm.coseencrypt0_headers(this.__wbg_ptr);
        return Headers.__wrap(ret);
    }
    /**
     * @returns {Uint8Array | undefined}
     */
    ciphertext() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.coseencrypt0_ciphertext(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            let v1;
            if (r0 !== 0) {
                v1 = getArrayU8FromWasm0(r0, r1).slice();
                wasm.__wbindgen_free(r0, r1 * 1, 1);
            }
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Headers} headers
     * @param {Uint8Array | null} [ciphertext]
     * @returns {COSERecipient}
     */
    static new(headers, ciphertext) {
        _assertClass(headers, Headers);
        var ptr0 = isLikeNone(ciphertext) ? 0 : passArray8ToWasm0(ciphertext, wasm.__wbindgen_malloc);
        var len0 = WASM_VECTOR_LEN;
        const ret = wasm.coseencrypt0_new(headers.__wbg_ptr, ptr0, len0);
        return COSERecipient.__wrap(ret);
    }
}

const COSERecipientsFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_coserecipients_free(ptr >>> 0, 1));

export class COSERecipients {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(COSERecipients.prototype);
        obj.__wbg_ptr = ptr;
        COSERecipientsFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        COSERecipientsFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_coserecipients_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.coserecipients_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {COSERecipients}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.coserecipients_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return COSERecipients.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {COSERecipients}
     */
    static new() {
        const ret = wasm.coserecipients_new();
        return COSERecipients.__wrap(ret);
    }
    /**
     * @returns {number}
     */
    len() {
        const ret = wasm.coserecipients_len(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
     * @param {number} index
     * @returns {COSERecipient}
     */
    get(index) {
        const ret = wasm.coserecipients_get(this.__wbg_ptr, index);
        return COSERecipient.__wrap(ret);
    }
    /**
     * @param {COSERecipient} elem
     */
    add(elem) {
        _assertClass(elem, COSERecipient);
        wasm.coserecipients_add(this.__wbg_ptr, elem.__wbg_ptr);
    }
}

const COSESignFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cosesign_free(ptr >>> 0, 1));

export class COSESign {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(COSESign.prototype);
        obj.__wbg_ptr = ptr;
        COSESignFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        COSESignFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_cosesign_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cosesign_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {COSESign}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.cosesign_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return COSESign.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {Headers}
     */
    headers() {
        const ret = wasm.cosesign_headers(this.__wbg_ptr);
        return Headers.__wrap(ret);
    }
    /**
     * @returns {Uint8Array | undefined}
     */
    payload() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cosesign_payload(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            let v1;
            if (r0 !== 0) {
                v1 = getArrayU8FromWasm0(r0, r1).slice();
                wasm.__wbindgen_free(r0, r1 * 1, 1);
            }
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {COSESignatures}
     */
    signatures() {
        const ret = wasm.cosesign_signatures(this.__wbg_ptr);
        return COSESignatures.__wrap(ret);
    }
    /**
     * @param {Headers} headers
     * @param {Uint8Array | null | undefined} payload
     * @param {COSESignatures} signatures
     * @returns {COSESign}
     */
    static new(headers, payload, signatures) {
        _assertClass(headers, Headers);
        var ptr0 = isLikeNone(payload) ? 0 : passArray8ToWasm0(payload, wasm.__wbindgen_malloc);
        var len0 = WASM_VECTOR_LEN;
        _assertClass(signatures, COSESignatures);
        const ret = wasm.cosesign_new(headers.__wbg_ptr, ptr0, len0, signatures.__wbg_ptr);
        return COSESign.__wrap(ret);
    }
}

const COSESign1Finalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cosesign1_free(ptr >>> 0, 1));

export class COSESign1 {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(COSESign1.prototype);
        obj.__wbg_ptr = ptr;
        COSESign1Finalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        COSESign1Finalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_cosesign1_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cosesign1_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {COSESign1}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.cosesign1_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return COSESign1.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {Headers}
     */
    headers() {
        const ret = wasm.cosesign1_headers(this.__wbg_ptr);
        return Headers.__wrap(ret);
    }
    /**
     * @returns {Uint8Array | undefined}
     */
    payload() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cosesign1_payload(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            let v1;
            if (r0 !== 0) {
                v1 = getArrayU8FromWasm0(r0, r1).slice();
                wasm.__wbindgen_free(r0, r1 * 1, 1);
            }
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {Uint8Array}
     */
    signature() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cosesign1_signature(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * For verifying, we will want to reverse-construct this SigStructure to check the signature against
     * # Arguments
     * * `external_aad` - External application data - see RFC 8152 section 4.3. Set to None if not using this.
     * @param {Uint8Array | null} [external_aad]
     * @param {Uint8Array | null} [external_payload]
     * @returns {SigStructure}
     */
    signed_data(external_aad, external_payload) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            var ptr0 = isLikeNone(external_aad) ? 0 : passArray8ToWasm0(external_aad, wasm.__wbindgen_malloc);
            var len0 = WASM_VECTOR_LEN;
            var ptr1 = isLikeNone(external_payload) ? 0 : passArray8ToWasm0(external_payload, wasm.__wbindgen_malloc);
            var len1 = WASM_VECTOR_LEN;
            wasm.cosesign1_signed_data(retptr, this.__wbg_ptr, ptr0, len0, ptr1, len1);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return SigStructure.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Headers} headers
     * @param {Uint8Array | null | undefined} payload
     * @param {Uint8Array} signature
     * @returns {COSESign1}
     */
    static new(headers, payload, signature) {
        _assertClass(headers, Headers);
        var ptr0 = isLikeNone(payload) ? 0 : passArray8ToWasm0(payload, wasm.__wbindgen_malloc);
        var len0 = WASM_VECTOR_LEN;
        const ptr1 = passArray8ToWasm0(signature, wasm.__wbindgen_malloc);
        const len1 = WASM_VECTOR_LEN;
        const ret = wasm.cosesign1_new(headers.__wbg_ptr, ptr0, len0, ptr1, len1);
        return COSESign1.__wrap(ret);
    }
}

const COSESign1BuilderFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cosesign1builder_free(ptr >>> 0, 1));

export class COSESign1Builder {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(COSESign1Builder.prototype);
        obj.__wbg_ptr = ptr;
        COSESign1BuilderFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        COSESign1BuilderFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_cosesign1builder_free(ptr, 0);
    }
    /**
     * @param {Headers} headers
     * @param {Uint8Array} payload
     * @param {boolean} is_payload_external
     * @returns {COSESign1Builder}
     */
    static new(headers, payload, is_payload_external) {
        _assertClass(headers, Headers);
        const ptr0 = passArray8ToWasm0(payload, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.cosesign1builder_new(headers.__wbg_ptr, ptr0, len0, is_payload_external);
        return COSESign1Builder.__wrap(ret);
    }
    hash_payload() {
        wasm.cosesign1builder_hash_payload(this.__wbg_ptr);
    }
    /**
     * @param {Uint8Array} external_aad
     */
    set_external_aad(external_aad) {
        const ptr0 = passArray8ToWasm0(external_aad, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.cosesign1builder_set_external_aad(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * @returns {SigStructure}
     */
    make_data_to_sign() {
        const ret = wasm.cosesign1builder_make_data_to_sign(this.__wbg_ptr);
        return SigStructure.__wrap(ret);
    }
    /**
     * @param {Uint8Array} signed_sig_structure
     * @returns {COSESign1}
     */
    build(signed_sig_structure) {
        const ptr0 = passArray8ToWasm0(signed_sig_structure, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.cosesign1builder_build(this.__wbg_ptr, ptr0, len0);
        return COSESign1.__wrap(ret);
    }
}

const COSESignBuilderFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cosesignbuilder_free(ptr >>> 0, 1));

export class COSESignBuilder {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(COSESignBuilder.prototype);
        obj.__wbg_ptr = ptr;
        COSESignBuilderFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        COSESignBuilderFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_cosesignbuilder_free(ptr, 0);
    }
    /**
     * @param {Headers} headers
     * @param {Uint8Array} payload
     * @param {boolean} is_payload_external
     * @returns {COSESignBuilder}
     */
    static new(headers, payload, is_payload_external) {
        _assertClass(headers, Headers);
        const ptr0 = passArray8ToWasm0(payload, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.cosesignbuilder_new(headers.__wbg_ptr, ptr0, len0, is_payload_external);
        return COSESignBuilder.__wrap(ret);
    }
    hash_payload() {
        wasm.cosesign1builder_hash_payload(this.__wbg_ptr);
    }
    /**
     * @param {Uint8Array} external_aad
     */
    set_external_aad(external_aad) {
        const ptr0 = passArray8ToWasm0(external_aad, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.cosesign1builder_set_external_aad(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * @returns {SigStructure}
     */
    make_data_to_sign() {
        const ret = wasm.cosesignbuilder_make_data_to_sign(this.__wbg_ptr);
        return SigStructure.__wrap(ret);
    }
    /**
     * @param {COSESignatures} signed_sig_structure
     * @returns {COSESign}
     */
    build(signed_sig_structure) {
        _assertClass(signed_sig_structure, COSESignatures);
        const ret = wasm.cosesignbuilder_build(this.__wbg_ptr, signed_sig_structure.__wbg_ptr);
        return COSESign.__wrap(ret);
    }
}

const COSESignatureFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cosesignature_free(ptr >>> 0, 1));

export class COSESignature {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(COSESignature.prototype);
        obj.__wbg_ptr = ptr;
        COSESignatureFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        COSESignatureFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_cosesignature_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cosesignature_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {COSESignature}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.cosesignature_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return COSESignature.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {Headers}
     */
    headers() {
        const ret = wasm.cosesignature_headers(this.__wbg_ptr);
        return Headers.__wrap(ret);
    }
    /**
     * @returns {Uint8Array}
     */
    signature() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cosesignature_signature(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Headers} headers
     * @param {Uint8Array} signature
     * @returns {COSESignature}
     */
    static new(headers, signature) {
        _assertClass(headers, Headers);
        const ptr0 = passArray8ToWasm0(signature, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.cosesignature_new(headers.__wbg_ptr, ptr0, len0);
        return COSESignature.__wrap(ret);
    }
}

const COSESignaturesFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_cosesignatures_free(ptr >>> 0, 1));

export class COSESignatures {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(COSESignatures.prototype);
        obj.__wbg_ptr = ptr;
        COSESignaturesFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        COSESignaturesFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_cosesignatures_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.cosesignatures_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {COSESignatures}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.cosesignatures_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return COSESignatures.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {COSESignatures}
     */
    static new() {
        const ret = wasm.coserecipients_new();
        return COSESignatures.__wrap(ret);
    }
    /**
     * @returns {number}
     */
    len() {
        const ret = wasm.cosesignatures_len(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
     * @param {number} index
     * @returns {COSESignature}
     */
    get(index) {
        const ret = wasm.cosesignatures_get(this.__wbg_ptr, index);
        return COSESignature.__wrap(ret);
    }
    /**
     * @param {COSESignature} elem
     */
    add(elem) {
        _assertClass(elem, COSESignature);
        wasm.cosesignatures_add(this.__wbg_ptr, elem.__wbg_ptr);
    }
}

const CounterSignatureFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_countersignature_free(ptr >>> 0, 1));

export class CounterSignature {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(CounterSignature.prototype);
        obj.__wbg_ptr = ptr;
        CounterSignatureFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        CounterSignatureFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_countersignature_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.countersignature_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {CounterSignature}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.countersignature_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return CounterSignature.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {COSESignature} cose_signature
     * @returns {CounterSignature}
     */
    static new_single(cose_signature) {
        _assertClass(cose_signature, COSESignature);
        const ret = wasm.countersignature_new_single(cose_signature.__wbg_ptr);
        return CounterSignature.__wrap(ret);
    }
    /**
     * @param {COSESignatures} cose_signatures
     * @returns {CounterSignature}
     */
    static new_multi(cose_signatures) {
        _assertClass(cose_signatures, COSESignatures);
        const ret = wasm.countersignature_new_multi(cose_signatures.__wbg_ptr);
        return CounterSignature.__wrap(ret);
    }
    /**
     * @returns {COSESignatures}
     */
    signatures() {
        const ret = wasm.countersignature_new_multi(this.__wbg_ptr);
        return COSESignatures.__wrap(ret);
    }
}

const EdDSA25519KeyFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_eddsa25519key_free(ptr >>> 0, 1));

export class EdDSA25519Key {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(EdDSA25519Key.prototype);
        obj.__wbg_ptr = ptr;
        EdDSA25519KeyFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        EdDSA25519KeyFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_eddsa25519key_free(ptr, 0);
    }
    /**
     * @param {Uint8Array} pubkey_bytes
     * @returns {EdDSA25519Key}
     */
    static new(pubkey_bytes) {
        const ptr0 = passArray8ToWasm0(pubkey_bytes, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.eddsa25519key_new(ptr0, len0);
        return EdDSA25519Key.__wrap(ret);
    }
    /**
     * @param {Uint8Array} private_key_bytes
     */
    set_private_key(private_key_bytes) {
        const ptr0 = passArray8ToWasm0(private_key_bytes, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.eddsa25519key_set_private_key(this.__wbg_ptr, ptr0, len0);
    }
    is_for_signing() {
        wasm.eddsa25519key_is_for_signing(this.__wbg_ptr);
    }
    is_for_verifying() {
        wasm.eddsa25519key_is_for_verifying(this.__wbg_ptr);
    }
    /**
     * @returns {COSEKey}
     */
    build() {
        const ret = wasm.eddsa25519key_build(this.__wbg_ptr);
        return COSEKey.__wrap(ret);
    }
}

const HeaderMapFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_headermap_free(ptr >>> 0, 1));

export class HeaderMap {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(HeaderMap.prototype);
        obj.__wbg_ptr = ptr;
        HeaderMapFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        HeaderMapFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_headermap_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.headermap_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {HeaderMap}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.headermap_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return HeaderMap.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Label} algorithm_id
     */
    set_algorithm_id(algorithm_id) {
        _assertClass(algorithm_id, Label);
        wasm.headermap_set_algorithm_id(this.__wbg_ptr, algorithm_id.__wbg_ptr);
    }
    /**
     * @returns {Label | undefined}
     */
    algorithm_id() {
        const ret = wasm.headermap_algorithm_id(this.__wbg_ptr);
        return ret === 0 ? undefined : Label.__wrap(ret);
    }
    /**
     * @param {Labels} criticality
     */
    set_criticality(criticality) {
        _assertClass(criticality, Labels);
        wasm.headermap_set_criticality(this.__wbg_ptr, criticality.__wbg_ptr);
    }
    /**
     * @returns {Labels | undefined}
     */
    criticality() {
        const ret = wasm.headermap_criticality(this.__wbg_ptr);
        return ret === 0 ? undefined : Labels.__wrap(ret);
    }
    /**
     * @param {Label} content_type
     */
    set_content_type(content_type) {
        _assertClass(content_type, Label);
        wasm.headermap_set_content_type(this.__wbg_ptr, content_type.__wbg_ptr);
    }
    /**
     * @returns {Label | undefined}
     */
    content_type() {
        const ret = wasm.headermap_content_type(this.__wbg_ptr);
        return ret === 0 ? undefined : Label.__wrap(ret);
    }
    /**
     * @param {Uint8Array} key_id
     */
    set_key_id(key_id) {
        const ptr0 = passArray8ToWasm0(key_id, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.headermap_set_key_id(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * @returns {Uint8Array | undefined}
     */
    key_id() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.headermap_key_id(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            let v1;
            if (r0 !== 0) {
                v1 = getArrayU8FromWasm0(r0, r1).slice();
                wasm.__wbindgen_free(r0, r1 * 1, 1);
            }
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} init_vector
     */
    set_init_vector(init_vector) {
        const ptr0 = passArray8ToWasm0(init_vector, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.headermap_set_init_vector(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * @returns {Uint8Array | undefined}
     */
    init_vector() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.headermap_init_vector(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            let v1;
            if (r0 !== 0) {
                v1 = getArrayU8FromWasm0(r0, r1).slice();
                wasm.__wbindgen_free(r0, r1 * 1, 1);
            }
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} partial_init_vector
     */
    set_partial_init_vector(partial_init_vector) {
        const ptr0 = passArray8ToWasm0(partial_init_vector, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.headermap_set_partial_init_vector(this.__wbg_ptr, ptr0, len0);
    }
    /**
     * @returns {Uint8Array | undefined}
     */
    partial_init_vector() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.headermap_partial_init_vector(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            let v1;
            if (r0 !== 0) {
                v1 = getArrayU8FromWasm0(r0, r1).slice();
                wasm.__wbindgen_free(r0, r1 * 1, 1);
            }
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {CounterSignature} counter_signature
     */
    set_counter_signature(counter_signature) {
        _assertClass(counter_signature, CounterSignature);
        wasm.headermap_set_counter_signature(this.__wbg_ptr, counter_signature.__wbg_ptr);
    }
    /**
     * @returns {CounterSignature | undefined}
     */
    counter_signature() {
        const ret = wasm.headermap_counter_signature(this.__wbg_ptr);
        return ret === 0 ? undefined : CounterSignature.__wrap(ret);
    }
    /**
     * @param {Label} label
     * @returns {CBORValue | undefined}
     */
    header(label) {
        _assertClass(label, Label);
        const ret = wasm.headermap_header(this.__wbg_ptr, label.__wbg_ptr);
        return ret === 0 ? undefined : CBORValue.__wrap(ret);
    }
    /**
     * @param {Label} label
     * @param {CBORValue} value
     */
    set_header(label, value) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            _assertClass(label, Label);
            _assertClass(value, CBORValue);
            wasm.headermap_set_header(retptr, this.__wbg_ptr, label.__wbg_ptr, value.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            if (r1) {
                throw takeObject(r0);
            }
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {Labels}
     */
    keys() {
        const ret = wasm.headermap_keys(this.__wbg_ptr);
        return Labels.__wrap(ret);
    }
    /**
     * @returns {HeaderMap}
     */
    static new() {
        const ret = wasm.headermap_new();
        return HeaderMap.__wrap(ret);
    }
}

const HeadersFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_headers_free(ptr >>> 0, 1));

export class Headers {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(Headers.prototype);
        obj.__wbg_ptr = ptr;
        HeadersFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        HeadersFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_headers_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.headers_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {Headers}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.headers_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return Headers.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {ProtectedHeaderMap}
     */
    protected() {
        const ret = wasm.headers_protected(this.__wbg_ptr);
        return ProtectedHeaderMap.__wrap(ret);
    }
    /**
     * @returns {HeaderMap}
     */
    unprotected() {
        const ret = wasm.headers_unprotected(this.__wbg_ptr);
        return HeaderMap.__wrap(ret);
    }
    /**
     * @param {ProtectedHeaderMap} protected_
     * @param {HeaderMap} unprotected_
     * @returns {Headers}
     */
    static new(protected_, unprotected_) {
        _assertClass(protected_, ProtectedHeaderMap);
        _assertClass(unprotected_, HeaderMap);
        const ret = wasm.headers_new(protected_.__wbg_ptr, unprotected_.__wbg_ptr);
        return Headers.__wrap(ret);
    }
}

const IntFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_int_free(ptr >>> 0, 1));

export class Int {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(Int.prototype);
        obj.__wbg_ptr = ptr;
        IntFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        IntFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_int_free(ptr, 0);
    }
    /**
     * @param {BigNum} x
     * @returns {Int}
     */
    static new(x) {
        _assertClass(x, BigNum);
        var ptr0 = x.__destroy_into_raw();
        const ret = wasm.int_new(ptr0);
        return Int.__wrap(ret);
    }
    /**
     * @param {BigNum} x
     * @returns {Int}
     */
    static new_negative(x) {
        _assertClass(x, BigNum);
        var ptr0 = x.__destroy_into_raw();
        const ret = wasm.int_new_negative(ptr0);
        return Int.__wrap(ret);
    }
    /**
     * @param {number} x
     * @returns {Int}
     */
    static new_i32(x) {
        const ret = wasm.int_new_i32(x);
        return Int.__wrap(ret);
    }
    /**
     * @returns {boolean}
     */
    is_positive() {
        const ret = wasm.int_is_positive(this.__wbg_ptr);
        return ret !== 0;
    }
    /**
     * @returns {BigNum | undefined}
     */
    as_positive() {
        const ret = wasm.int_as_positive(this.__wbg_ptr);
        return ret === 0 ? undefined : BigNum.__wrap(ret);
    }
    /**
     * @returns {BigNum | undefined}
     */
    as_negative() {
        const ret = wasm.int_as_negative(this.__wbg_ptr);
        return ret === 0 ? undefined : BigNum.__wrap(ret);
    }
    /**
     * @returns {number | undefined}
     */
    as_i32() {
        const ret = wasm.int_as_i32(this.__wbg_ptr);
        return ret === 0x100000001 ? undefined : ret;
    }
}

const LabelFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_label_free(ptr >>> 0, 1));

export class Label {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(Label.prototype);
        obj.__wbg_ptr = ptr;
        LabelFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        LabelFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_label_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.label_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {Label}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.label_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return Label.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Int} int
     * @returns {Label}
     */
    static new_int(int) {
        _assertClass(int, Int);
        const ret = wasm.label_new_int(int.__wbg_ptr);
        return Label.__wrap(ret);
    }
    /**
     * @param {string} text
     * @returns {Label}
     */
    static new_text(text) {
        const ptr0 = passStringToWasm0(text, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.label_new_text(ptr0, len0);
        return Label.__wrap(ret);
    }
    /**
     * @returns {LabelKind}
     */
    kind() {
        const ret = wasm.label_kind(this.__wbg_ptr);
        return ret;
    }
    /**
     * @returns {Int | undefined}
     */
    as_int() {
        const ret = wasm.label_as_int(this.__wbg_ptr);
        return ret === 0 ? undefined : Int.__wrap(ret);
    }
    /**
     * @returns {string | undefined}
     */
    as_text() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.label_as_text(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            let v1;
            if (r0 !== 0) {
                v1 = getStringFromWasm0(r0, r1).slice();
                wasm.__wbindgen_free(r0, r1 * 1, 1);
            }
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {AlgorithmId} id
     * @returns {Label}
     */
    static from_algorithm_id(id) {
        const ret = wasm.label_from_algorithm_id(id);
        return Label.__wrap(ret);
    }
    /**
     * @param {KeyType} key_type
     * @returns {Label}
     */
    static from_key_type(key_type) {
        const ret = wasm.label_from_key_type(key_type);
        return Label.__wrap(ret);
    }
    /**
     * @param {ECKey} ec_key
     * @returns {Label}
     */
    static from_ec_key(ec_key) {
        const ret = wasm.label_from_ec_key(ec_key);
        return Label.__wrap(ret);
    }
    /**
     * @param {CurveType} curve_type
     * @returns {Label}
     */
    static from_curve_type(curve_type) {
        const ret = wasm.label_from_curve_type(curve_type);
        return Label.__wrap(ret);
    }
    /**
     * @param {KeyOperation} key_op
     * @returns {Label}
     */
    static from_key_operation(key_op) {
        const ret = wasm.label_from_key_operation(key_op);
        return Label.__wrap(ret);
    }
}

const LabelsFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_labels_free(ptr >>> 0, 1));

export class Labels {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(Labels.prototype);
        obj.__wbg_ptr = ptr;
        LabelsFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        LabelsFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_labels_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.labels_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {Labels}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.labels_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return Labels.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {Labels}
     */
    static new() {
        const ret = wasm.coserecipients_new();
        return Labels.__wrap(ret);
    }
    /**
     * @returns {number}
     */
    len() {
        const ret = wasm.labels_len(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
     * @param {number} index
     * @returns {Label}
     */
    get(index) {
        const ret = wasm.labels_get(this.__wbg_ptr, index);
        return Label.__wrap(ret);
    }
    /**
     * @param {Label} elem
     */
    add(elem) {
        _assertClass(elem, Label);
        wasm.labels_add(this.__wbg_ptr, elem.__wbg_ptr);
    }
}

const PasswordEncryptionFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_passwordencryption_free(ptr >>> 0, 1));

export class PasswordEncryption {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(PasswordEncryption.prototype);
        obj.__wbg_ptr = ptr;
        PasswordEncryptionFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        PasswordEncryptionFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_passwordencryption_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.passwordencryption_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {PasswordEncryption}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.passwordencryption_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return PasswordEncryption.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {COSEEncrypt0} data
     * @returns {PasswordEncryption}
     */
    static new(data) {
        _assertClass(data, COSEEncrypt0);
        const ret = wasm.passwordencryption_new(data.__wbg_ptr);
        return PasswordEncryption.__wrap(ret);
    }
}

const ProtectedHeaderMapFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_protectedheadermap_free(ptr >>> 0, 1));

export class ProtectedHeaderMap {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(ProtectedHeaderMap.prototype);
        obj.__wbg_ptr = ptr;
        ProtectedHeaderMapFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        ProtectedHeaderMapFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_protectedheadermap_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.protectedheadermap_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {ProtectedHeaderMap}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.protectedheadermap_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return ProtectedHeaderMap.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {ProtectedHeaderMap}
     */
    static new_empty() {
        const ret = wasm.protectedheadermap_new_empty();
        return ProtectedHeaderMap.__wrap(ret);
    }
    /**
     * @param {HeaderMap} header_map
     * @returns {ProtectedHeaderMap}
     */
    static new(header_map) {
        _assertClass(header_map, HeaderMap);
        const ret = wasm.protectedheadermap_new(header_map.__wbg_ptr);
        return ProtectedHeaderMap.__wrap(ret);
    }
    /**
     * @returns {HeaderMap}
     */
    deserialized_headers() {
        const ret = wasm.protectedheadermap_deserialized_headers(this.__wbg_ptr);
        return HeaderMap.__wrap(ret);
    }
}

const PubKeyEncryptionFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_pubkeyencryption_free(ptr >>> 0, 1));

export class PubKeyEncryption {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(PubKeyEncryption.prototype);
        obj.__wbg_ptr = ptr;
        PubKeyEncryptionFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        PubKeyEncryptionFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_pubkeyencryption_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.pubkeyencryption_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {PubKeyEncryption}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.pubkeyencryption_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return PubKeyEncryption.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {COSEEncrypt} data
     * @returns {PubKeyEncryption}
     */
    static new(data) {
        _assertClass(data, COSEEncrypt);
        const ret = wasm.pubkeyencryption_new(data.__wbg_ptr);
        return PubKeyEncryption.__wrap(ret);
    }
}

const SigStructureFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_sigstructure_free(ptr >>> 0, 1));

export class SigStructure {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(SigStructure.prototype);
        obj.__wbg_ptr = ptr;
        SigStructureFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        SigStructureFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_sigstructure_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.sigstructure_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {SigStructure}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.sigstructure_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return SigStructure.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {SigContext}
     */
    context() {
        const ret = wasm.sigstructure_context(this.__wbg_ptr);
        return ret;
    }
    /**
     * @returns {ProtectedHeaderMap}
     */
    body_protected() {
        const ret = wasm.sigstructure_body_protected(this.__wbg_ptr);
        return ProtectedHeaderMap.__wrap(ret);
    }
    /**
     * @returns {ProtectedHeaderMap | undefined}
     */
    sign_protected() {
        const ret = wasm.sigstructure_sign_protected(this.__wbg_ptr);
        return ret === 0 ? undefined : ProtectedHeaderMap.__wrap(ret);
    }
    /**
     * @returns {Uint8Array}
     */
    external_aad() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.sigstructure_external_aad(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {Uint8Array}
     */
    payload() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.sigstructure_payload(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {ProtectedHeaderMap} sign_protected
     */
    set_sign_protected(sign_protected) {
        _assertClass(sign_protected, ProtectedHeaderMap);
        wasm.sigstructure_set_sign_protected(this.__wbg_ptr, sign_protected.__wbg_ptr);
    }
    /**
     * @param {SigContext} context
     * @param {ProtectedHeaderMap} body_protected
     * @param {Uint8Array} external_aad
     * @param {Uint8Array} payload
     * @returns {SigStructure}
     */
    static new(context, body_protected, external_aad, payload) {
        _assertClass(body_protected, ProtectedHeaderMap);
        const ptr0 = passArray8ToWasm0(external_aad, wasm.__wbindgen_malloc);
        const len0 = WASM_VECTOR_LEN;
        const ptr1 = passArray8ToWasm0(payload, wasm.__wbindgen_malloc);
        const len1 = WASM_VECTOR_LEN;
        const ret = wasm.sigstructure_new(context, body_protected.__wbg_ptr, ptr0, len0, ptr1, len1);
        return SigStructure.__wrap(ret);
    }
}

const SignedMessageFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_signedmessage_free(ptr >>> 0, 1));

export class SignedMessage {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(SignedMessage.prototype);
        obj.__wbg_ptr = ptr;
        SignedMessageFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        SignedMessageFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_signedmessage_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.signedmessage_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {SignedMessage}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.signedmessage_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return SignedMessage.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {COSESign} cose_sign
     * @returns {SignedMessage}
     */
    static new_cose_sign(cose_sign) {
        _assertClass(cose_sign, COSESign);
        const ret = wasm.signedmessage_new_cose_sign(cose_sign.__wbg_ptr);
        return SignedMessage.__wrap(ret);
    }
    /**
     * @param {COSESign1} cose_sign1
     * @returns {SignedMessage}
     */
    static new_cose_sign1(cose_sign1) {
        _assertClass(cose_sign1, COSESign1);
        const ret = wasm.signedmessage_new_cose_sign1(cose_sign1.__wbg_ptr);
        return SignedMessage.__wrap(ret);
    }
    /**
     * @param {string} s
     * @returns {SignedMessage}
     */
    static from_user_facing_encoding(s) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passStringToWasm0(s, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.signedmessage_from_user_facing_encoding(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return SignedMessage.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {string}
     */
    to_user_facing_encoding() {
        let deferred1_0;
        let deferred1_1;
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.signedmessage_to_user_facing_encoding(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            deferred1_0 = r0;
            deferred1_1 = r1;
            return getStringFromWasm0(r0, r1);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
    /**
     * @returns {SignedMessageKind}
     */
    kind() {
        const ret = wasm.signedmessage_kind(this.__wbg_ptr);
        return ret;
    }
    /**
     * @returns {COSESign | undefined}
     */
    as_cose_sign() {
        const ret = wasm.signedmessage_as_cose_sign(this.__wbg_ptr);
        return ret === 0 ? undefined : COSESign.__wrap(ret);
    }
    /**
     * @returns {COSESign1 | undefined}
     */
    as_cose_sign1() {
        const ret = wasm.signedmessage_as_cose_sign1(this.__wbg_ptr);
        return ret === 0 ? undefined : COSESign1.__wrap(ret);
    }
}

const TaggedCBORFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_taggedcbor_free(ptr >>> 0, 1));

export class TaggedCBOR {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(TaggedCBOR.prototype);
        obj.__wbg_ptr = ptr;
        TaggedCBORFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        TaggedCBORFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_taggedcbor_free(ptr, 0);
    }
    /**
     * @returns {Uint8Array}
     */
    to_bytes() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.taggedcbor_to_bytes(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var v1 = getArrayU8FromWasm0(r0, r1).slice();
            wasm.__wbindgen_free(r0, r1 * 1, 1);
            return v1;
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @param {Uint8Array} bytes
     * @returns {TaggedCBOR}
     */
    static from_bytes(bytes) {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            const ptr0 = passArray8ToWasm0(bytes, wasm.__wbindgen_malloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.taggedcbor_from_bytes(retptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return TaggedCBOR.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
     * @returns {BigNum}
     */
    tag() {
        const ret = wasm.taggedcbor_tag(this.__wbg_ptr);
        return BigNum.__wrap(ret);
    }
    /**
     * @returns {CBORValue}
     */
    value() {
        const ret = wasm.taggedcbor_value(this.__wbg_ptr);
        return CBORValue.__wrap(ret);
    }
    /**
     * @param {BigNum} tag
     * @param {CBORValue} value
     * @returns {TaggedCBOR}
     */
    static new(tag, value) {
        _assertClass(tag, BigNum);
        var ptr0 = tag.__destroy_into_raw();
        _assertClass(value, CBORValue);
        const ret = wasm.taggedcbor_new(ptr0, value.__wbg_ptr);
        return TaggedCBOR.__wrap(ret);
    }
}

export function __wbindgen_debug_string(arg0, arg1) {
    const ret = debugString(getObject(arg1));
    const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len1 = WASM_VECTOR_LEN;
    getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
    getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
};

export function __wbindgen_object_drop_ref(arg0) {
    takeObject(arg0);
};

export function __wbindgen_string_new(arg0, arg1) {
    const ret = getStringFromWasm0(arg0, arg1);
    return addHeapObject(ret);
};

export function __wbindgen_throw(arg0, arg1) {
    throw new Error(getStringFromWasm0(arg0, arg1));
};

