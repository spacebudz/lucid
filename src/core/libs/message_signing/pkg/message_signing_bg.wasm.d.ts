/* tslint:disable */
/* eslint-disable */
export const memory: WebAssembly.Memory;
export const __wbg_cosesign1builder_free: (a: number, b: number) => void;
export const cosesign1builder_new: (a: number, b: number, c: number, d: number) => number;
export const cosesign1builder_hash_payload: (a: number) => void;
export const cosesign1builder_set_external_aad: (a: number, b: number, c: number) => void;
export const cosesign1builder_make_data_to_sign: (a: number) => number;
export const cosesign1builder_build: (a: number, b: number, c: number) => number;
export const cosesignbuilder_new: (a: number, b: number, c: number, d: number) => number;
export const cosesignbuilder_make_data_to_sign: (a: number) => number;
export const cosesignbuilder_build: (a: number, b: number) => number;
export const __wbg_eddsa25519key_free: (a: number, b: number) => void;
export const eddsa25519key_new: (a: number, b: number) => number;
export const eddsa25519key_set_private_key: (a: number, b: number, c: number) => void;
export const eddsa25519key_is_for_signing: (a: number) => void;
export const eddsa25519key_is_for_verifying: (a: number) => void;
export const eddsa25519key_build: (a: number) => number;
export const __wbg_taggedcbor_free: (a: number, b: number) => void;
export const taggedcbor_to_bytes: (a: number, b: number) => void;
export const taggedcbor_from_bytes: (a: number, b: number, c: number) => void;
export const taggedcbor_tag: (a: number) => number;
export const taggedcbor_value: (a: number) => number;
export const taggedcbor_new: (a: number, b: number) => number;
export const __wbg_cborarray_free: (a: number, b: number) => void;
export const cborarray_to_bytes: (a: number, b: number) => void;
export const cborarray_from_bytes: (a: number, b: number, c: number) => void;
export const cborarray_new: () => number;
export const cborarray_len: (a: number) => number;
export const cborarray_get: (a: number, b: number) => number;
export const cborarray_add: (a: number, b: number) => void;
export const cborarray_set_definite_encoding: (a: number, b: number) => void;
export const cborarray_is_definite: (a: number) => number;
export const __wbg_cborobject_free: (a: number, b: number) => void;
export const cborobject_to_bytes: (a: number, b: number) => void;
export const cborobject_from_bytes: (a: number, b: number, c: number) => void;
export const cborobject_new: () => number;
export const cborobject_len: (a: number) => number;
export const cborobject_insert: (a: number, b: number, c: number) => number;
export const cborobject_get: (a: number, b: number) => number;
export const cborobject_keys: (a: number) => number;
export const cborobject_set_definite_encoding: (a: number, b: number) => void;
export const cborobject_is_definite: (a: number) => number;
export const __wbg_cborspecial_free: (a: number, b: number) => void;
export const cborspecial_to_bytes: (a: number, b: number) => void;
export const cborspecial_from_bytes: (a: number, b: number, c: number) => void;
export const cborspecial_new_bool: (a: number) => number;
export const cborspecial_new_unassigned: (a: number) => number;
export const cborspecial_new_break: () => number;
export const cborspecial_new_null: () => number;
export const cborspecial_new_undefined: () => number;
export const cborspecial_kind: (a: number) => number;
export const cborspecial_as_bool: (a: number) => number;
export const cborspecial_as_float: (a: number, b: number) => void;
export const cborspecial_as_unassigned: (a: number) => number;
export const __wbg_cborvalue_free: (a: number, b: number) => void;
export const cborvalue_to_bytes: (a: number, b: number) => void;
export const cborvalue_from_bytes: (a: number, b: number, c: number) => void;
export const cborvalue_new_int: (a: number) => number;
export const cborvalue_new_bytes: (a: number, b: number) => number;
export const cborvalue_new_text: (a: number, b: number) => number;
export const cborvalue_new_array: (a: number) => number;
export const cborvalue_new_object: (a: number) => number;
export const cborvalue_new_tagged: (a: number) => number;
export const cborvalue_new_special: (a: number) => number;
export const cborvalue_from_label: (a: number) => number;
export const cborvalue_kind: (a: number) => number;
export const cborvalue_as_int: (a: number) => number;
export const cborvalue_as_bytes: (a: number, b: number) => void;
export const cborvalue_as_text: (a: number, b: number) => void;
export const cborvalue_as_array: (a: number) => number;
export const cborvalue_as_object: (a: number) => number;
export const cborvalue_as_tagged: (a: number) => number;
export const cborvalue_as_special: (a: number) => number;
export const __wbg_bignum_free: (a: number, b: number) => void;
export const bignum_to_bytes: (a: number, b: number) => void;
export const bignum_from_bytes: (a: number, b: number, c: number) => void;
export const bignum_from_str: (a: number, b: number, c: number) => void;
export const bignum_to_str: (a: number, b: number) => void;
export const bignum_checked_mul: (a: number, b: number, c: number) => void;
export const bignum_checked_add: (a: number, b: number, c: number) => void;
export const bignum_checked_sub: (a: number, b: number, c: number) => void;
export const __wbg_int_free: (a: number, b: number) => void;
export const int_new: (a: number) => number;
export const int_new_negative: (a: number) => number;
export const int_new_i32: (a: number) => number;
export const int_is_positive: (a: number) => number;
export const int_as_positive: (a: number) => number;
export const int_as_negative: (a: number) => number;
export const int_as_i32: (a: number) => number;
export const __wbg_protectedheadermap_free: (a: number, b: number) => void;
export const protectedheadermap_to_bytes: (a: number, b: number) => void;
export const protectedheadermap_from_bytes: (a: number, b: number, c: number) => void;
export const protectedheadermap_new_empty: () => number;
export const protectedheadermap_new: (a: number) => number;
export const protectedheadermap_deserialized_headers: (a: number) => number;
export const __wbg_label_free: (a: number, b: number) => void;
export const label_to_bytes: (a: number, b: number) => void;
export const label_from_bytes: (a: number, b: number, c: number) => void;
export const label_new_int: (a: number) => number;
export const label_new_text: (a: number, b: number) => number;
export const label_kind: (a: number) => number;
export const label_as_int: (a: number) => number;
export const label_as_text: (a: number, b: number) => void;
export const label_from_algorithm_id: (a: number) => number;
export const label_from_key_type: (a: number) => number;
export const label_from_ec_key: (a: number) => number;
export const label_from_curve_type: (a: number) => number;
export const label_from_key_operation: (a: number) => number;
export const __wbg_labels_free: (a: number, b: number) => void;
export const labels_to_bytes: (a: number, b: number) => void;
export const labels_from_bytes: (a: number, b: number, c: number) => void;
export const labels_len: (a: number) => number;
export const labels_get: (a: number, b: number) => number;
export const labels_add: (a: number, b: number) => void;
export const __wbg_cosesignatures_free: (a: number, b: number) => void;
export const cosesignatures_to_bytes: (a: number, b: number) => void;
export const cosesignatures_from_bytes: (a: number, b: number, c: number) => void;
export const cosesignatures_len: (a: number) => number;
export const cosesignatures_get: (a: number, b: number) => number;
export const cosesignatures_add: (a: number, b: number) => void;
export const countersignature_to_bytes: (a: number, b: number) => void;
export const countersignature_from_bytes: (a: number, b: number, c: number) => void;
export const countersignature_new_single: (a: number) => number;
export const countersignature_new_multi: (a: number) => number;
export const __wbg_headermap_free: (a: number, b: number) => void;
export const headermap_to_bytes: (a: number, b: number) => void;
export const headermap_from_bytes: (a: number, b: number, c: number) => void;
export const headermap_set_algorithm_id: (a: number, b: number) => void;
export const headermap_algorithm_id: (a: number) => number;
export const headermap_set_criticality: (a: number, b: number) => void;
export const headermap_criticality: (a: number) => number;
export const headermap_set_content_type: (a: number, b: number) => void;
export const headermap_content_type: (a: number) => number;
export const headermap_set_key_id: (a: number, b: number, c: number) => void;
export const headermap_key_id: (a: number, b: number) => void;
export const headermap_set_init_vector: (a: number, b: number, c: number) => void;
export const headermap_init_vector: (a: number, b: number) => void;
export const headermap_set_partial_init_vector: (a: number, b: number, c: number) => void;
export const headermap_partial_init_vector: (a: number, b: number) => void;
export const headermap_set_counter_signature: (a: number, b: number) => void;
export const headermap_counter_signature: (a: number) => number;
export const headermap_header: (a: number, b: number) => number;
export const headermap_set_header: (a: number, b: number, c: number, d: number) => void;
export const headermap_keys: (a: number) => number;
export const headermap_new: () => number;
export const __wbg_headers_free: (a: number, b: number) => void;
export const headers_to_bytes: (a: number, b: number) => void;
export const headers_from_bytes: (a: number, b: number, c: number) => void;
export const headers_protected: (a: number) => number;
export const headers_unprotected: (a: number) => number;
export const headers_new: (a: number, b: number) => number;
export const __wbg_cosesignature_free: (a: number, b: number) => void;
export const cosesignature_to_bytes: (a: number, b: number) => void;
export const cosesignature_from_bytes: (a: number, b: number, c: number) => void;
export const cosesignature_headers: (a: number) => number;
export const cosesignature_signature: (a: number, b: number) => void;
export const cosesignature_new: (a: number, b: number, c: number) => number;
export const __wbg_cosesign1_free: (a: number, b: number) => void;
export const cosesign1_to_bytes: (a: number, b: number) => void;
export const cosesign1_from_bytes: (a: number, b: number, c: number) => void;
export const cosesign1_headers: (a: number) => number;
export const cosesign1_payload: (a: number, b: number) => void;
export const cosesign1_signature: (a: number, b: number) => void;
export const cosesign1_signed_data: (a: number, b: number, c: number, d: number, e: number, f: number) => void;
export const cosesign1_new: (a: number, b: number, c: number, d: number, e: number) => number;
export const __wbg_cosesign_free: (a: number, b: number) => void;
export const cosesign_to_bytes: (a: number, b: number) => void;
export const cosesign_from_bytes: (a: number, b: number, c: number) => void;
export const cosesign_headers: (a: number) => number;
export const cosesign_payload: (a: number, b: number) => void;
export const cosesign_signatures: (a: number) => number;
export const cosesign_new: (a: number, b: number, c: number, d: number) => number;
export const __wbg_signedmessage_free: (a: number, b: number) => void;
export const signedmessage_to_bytes: (a: number, b: number) => void;
export const signedmessage_from_bytes: (a: number, b: number, c: number) => void;
export const signedmessage_new_cose_sign: (a: number) => number;
export const signedmessage_new_cose_sign1: (a: number) => number;
export const signedmessage_from_user_facing_encoding: (a: number, b: number, c: number) => void;
export const signedmessage_to_user_facing_encoding: (a: number, b: number) => void;
export const signedmessage_kind: (a: number) => number;
export const signedmessage_as_cose_sign: (a: number) => number;
export const signedmessage_as_cose_sign1: (a: number) => number;
export const __wbg_sigstructure_free: (a: number, b: number) => void;
export const sigstructure_to_bytes: (a: number, b: number) => void;
export const sigstructure_from_bytes: (a: number, b: number, c: number) => void;
export const sigstructure_context: (a: number) => number;
export const sigstructure_body_protected: (a: number) => number;
export const sigstructure_sign_protected: (a: number) => number;
export const sigstructure_external_aad: (a: number, b: number) => void;
export const sigstructure_payload: (a: number, b: number) => void;
export const sigstructure_set_sign_protected: (a: number, b: number) => void;
export const sigstructure_new: (a: number, b: number, c: number, d: number, e: number, f: number) => number;
export const __wbg_coseencrypt0_free: (a: number, b: number) => void;
export const coseencrypt0_to_bytes: (a: number, b: number) => void;
export const coseencrypt0_from_bytes: (a: number, b: number, c: number) => void;
export const coseencrypt0_headers: (a: number) => number;
export const coseencrypt0_ciphertext: (a: number, b: number) => void;
export const coseencrypt0_new: (a: number, b: number, c: number) => number;
export const passwordencryption_to_bytes: (a: number, b: number) => void;
export const passwordencryption_from_bytes: (a: number, b: number, c: number) => void;
export const passwordencryption_new: (a: number) => number;
export const __wbg_coserecipients_free: (a: number, b: number) => void;
export const coserecipients_to_bytes: (a: number, b: number) => void;
export const coserecipients_from_bytes: (a: number, b: number, c: number) => void;
export const coserecipients_new: () => number;
export const coserecipients_len: (a: number) => number;
export const coserecipients_get: (a: number, b: number) => number;
export const coserecipients_add: (a: number, b: number) => void;
export const __wbg_coseencrypt_free: (a: number, b: number) => void;
export const coseencrypt_to_bytes: (a: number, b: number) => void;
export const coseencrypt_from_bytes: (a: number, b: number, c: number) => void;
export const coseencrypt_headers: (a: number) => number;
export const coseencrypt_ciphertext: (a: number, b: number) => void;
export const coseencrypt_recipients: (a: number) => number;
export const coseencrypt_new: (a: number, b: number, c: number, d: number) => number;
export const coserecipient_to_bytes: (a: number, b: number) => void;
export const coserecipient_from_bytes: (a: number, b: number, c: number) => void;
export const pubkeyencryption_to_bytes: (a: number, b: number) => void;
export const pubkeyencryption_from_bytes: (a: number, b: number, c: number) => void;
export const pubkeyencryption_new: (a: number) => number;
export const __wbg_cosekey_free: (a: number, b: number) => void;
export const cosekey_to_bytes: (a: number, b: number) => void;
export const cosekey_from_bytes: (a: number, b: number, c: number) => void;
export const cosekey_set_key_type: (a: number, b: number) => void;
export const cosekey_key_type: (a: number) => number;
export const cosekey_set_key_id: (a: number, b: number, c: number) => void;
export const cosekey_key_id: (a: number, b: number) => void;
export const cosekey_set_algorithm_id: (a: number, b: number) => void;
export const cosekey_algorithm_id: (a: number) => number;
export const cosekey_set_key_ops: (a: number, b: number) => void;
export const cosekey_key_ops: (a: number) => number;
export const cosekey_set_base_init_vector: (a: number, b: number, c: number) => void;
export const cosekey_base_init_vector: (a: number, b: number) => void;
export const cosekey_header: (a: number, b: number) => number;
export const cosekey_set_header: (a: number, b: number, c: number, d: number) => void;
export const cosekey_new: (a: number) => number;
export const coserecipient_new: (a: number, b: number, c: number) => number;
export const labels_new: () => number;
export const cosesignatures_new: () => number;
export const __wbg_countersignature_free: (a: number, b: number) => void;
export const countersignature_signatures: (a: number) => number;
export const __wbg_cosesignbuilder_free: (a: number, b: number) => void;
export const __wbg_pubkeyencryption_free: (a: number, b: number) => void;
export const __wbg_passwordencryption_free: (a: number, b: number) => void;
export const __wbg_coserecipient_free: (a: number, b: number) => void;
export const cosesignbuilder_set_external_aad: (a: number, b: number, c: number) => void;
export const cosesignbuilder_hash_payload: (a: number) => void;
export const coserecipient_ciphertext: (a: number, b: number) => void;
export const coserecipient_headers: (a: number) => number;
export const __wbindgen_malloc: (a: number, b: number) => number;
export const __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
export const __wbindgen_add_to_stack_pointer: (a: number) => number;
export const __wbindgen_free: (a: number, b: number, c: number) => void;
