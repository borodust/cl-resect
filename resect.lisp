(cl:defpackage :%resect
  (:use)
  (:export #:parse
           #:free

           #:collection-iterator
           #:iterator-next
           #:iterator-value
           #:iterator-free

           #:translation-unit-declarations

           #:declaration-kind
           #:declaration-id
           #:declaration-name
           #:declaration-location
           #:declaration-type

           #:location-name
           #:location-line
           #:location-column

           #:type-kind
           #:type-category
           #:type-name
           #:type-size
           #:type-alignment
           #:type-field-offset
           #:type-declaration

           #:array-size
           #:array-element-type

           #:pointer-pointee-type

           #:typedef-aliased-type

           #:enum-constants
           #:enum-constant-value

           #:function-parameters
           #:function-return-type
           #:function-variadic-p
           #:function-storage-class
           #:function-calling-convention

           #:field-offset
           #:field-bitfield-p
           #:field-width
           #:struct-fields
           #:union-fields

           #:make-options
           #:options-add-include-path
           #:options-add-framework-path
           #:options-add-language
           #:options-add-standard
           #:options-add-target
           #:destroy-options))
(cl:in-package :%resect)


(cffi:defcenum type-kind
  (:unknown 0)
  (:void 2)
  (:bool 3)
  (:char-u 4)
  (:unsigned-char 5)
  (:char16 6)
  (:char32 7)
  (:unsigned-short 8)
  (:unsigned-int 9)
  (:unsigned-long 10)
  (:unsigned-long-long 11)
  (:unsigned-int128 12)
  (:char-s 13)
  (:char 14)
  (:wchar 15)
  (:short 16)
  (:int 17)
  (:long 18)
  (:long-long 19)
  (:int128 20)
  (:float 21)
  (:double 22)
  (:long-double 23)
  (:nullptr 24)
  (:overload 25)
  (:dependent 26)
  (:float128 30)
  (:half 31)
  (:float16 32)
  (:complex 100)
  (:pointer 101)
  (:block-pointer 102)
  (:lvalue-reference 103)
  (:rvalue-reference 104)
  (:record 105)
  (:enum 106)
  (:typedef 107)
  (:function-no-prototype 110)
  (:function-prototype 111)
  (:constant-array 112)
  (:vector 113)
  (:incomplete-array 114)
  (:variable-array 115)
  (:dependent-sized-array 116)
  (:member-pointer 117)
  (:auto 118)
  (:attributed 163)
  (:extended-vector 178))


(cffi:defcenum type-category
  (:unknown 0)
  (:arithmetic 1)
  (:pointer 2)
  (:reference 3)
  (:array 4)
  (:unique 5)
  (:aux 6))


(cffi:defcenum declaration-kind
  (:unknown 0)
  (:struct 1)
  (:union 2)
  (:class 3)
  (:enum 4)
  (:field 5)
  (:function 6)
  (:variable 7)
  (:parameter 8)
  (:typedef 9)
  (:method 10)
  (:namespace 11)
  (:constructor 12)
  (:destructor 13)
  (:converter 14)
  (:type-reference 15)
  (:template-reference 16)
  (:enum-constant 17))


(cffi:defcenum calling-convention
  (:unknown 0)
  (:default 1)
  (:c 2)
  (:x86-stdcall 3)
  (:x86-fastcall 4)
  (:x86-thiscall 5)
  (:x86-regcall 6)
  (:x86-vectorcall 7)
  (:x86-pascal 8)
  (:x86-64-win64 9)
  (:x86-64-sysv 10)
  (:aarch64-vectorcall 11)
  (:aapcs 12)
  (:aapcs-vfp 13)
  (:intel-ocl-bicc 14)
  (:swift 15)
  (:reserve-most 16)
  (:reserve-all 17))


(cffi:defcenum storage-class
  (:unknown 0)
  (:none 1)
  (:extern 2)
  (:static 3)
  (:private-extern 4)
  (:opencl-workgroup-local 5)
  (:auto 6)
  (:register 7))


(cffi:defctype collection :pointer)
(cffi:defctype iterator :pointer)
(cffi:defctype type :pointer)
(cffi:defctype declaration :pointer)
(cffi:defctype location :pointer)
(cffi:defctype translation-unit :pointer)
(cffi:defctype options :pointer)


;;;
;;; COLLECTION
;;;
(cffi:defcfun (collection-iterator "resect_collection_iterator") iterator
  (collection collection))
(cffi:defcfun (iterator-next "resect_iterator_next") :boolean
  (iterator iterator))
(cffi:defcfun (iterator-value "resect_iterator_value") :pointer
  (iterator iterator))
(cffi:defcfun (iterator-free "resect_iterator_free") :void
  (iterator iterator))

;;;
;;; LOCATION
;;;
(cffi:defcfun (location-line "resect_location_line") :unsigned-int
  (location location))
(cffi:defcfun (location-column "resect_location_column") :unsigned-int
  (location location))
(cffi:defcfun (location-name "resect_location_name") :string
  (location location))

;;;
;;; TYPE
;;;
(cffi:defcfun (type-kind "resect_type_get_kind") type-kind
  (type type))
(cffi:defcfun (type-category "resect_type_get_category") type-category
  (type type))
(cffi:defcfun (type-name "resect_type_get_name") :string
  (type type))
(cffi:defcfun (type-size "resect_type_sizeof") :long-long
  (type type))
(cffi:defcfun (type-alignment "resect_type_alignof") :long-long
  (type type))
(cffi:defcfun (type-field-offset "resect_type_offsetof") :long-long
  (type type)
  (field :string))
(cffi:defcfun (type-declaration "resect_type_get_declaration") declaration
  (type type))

;;;
;;; ARRAY
;;;
(cffi:defcfun (array-size "resect_array_get_size") :long-long
  (type type))
(cffi:defcfun (array-element-type "resect_array_get_element_type") type
  (type type))

;;;
;;; POINTER
;;;
(cffi:defcfun (pointer-pointee-type "resect_pointer_get_pointee_type") type
  (type type))

;;;
;;; DECLARATION
;;;
(cffi:defcfun (declaration-kind "resect_decl_get_kind") declaration-kind
  (declaration declaration))
(cffi:defcfun (declaration-id "resect_decl_get_id") :string
  (declaration declaration))
(cffi:defcfun (declaration-location "resect_decl_get_location") location
  (declaration declaration))
(cffi:defcfun (declaration-name "resect_decl_get_name") :string
  (declaration declaration))
(cffi:defcfun (declaration-comment "resect_decl_get_comment") :string
  (declaration declaration))
(cffi:defcfun (declaration-type "resect_decl_get_type") type
  (declaration declaration))


;;;
;;; UNIT
;;;
(cffi:defcfun (translation-unit-declarations "resect_unit_declarations") collection
  (unit translation-unit))


(cffi:defcfun (field-offset "resect_field_get_offset") :long-long
  (record declaration))
(cffi:defcfun (field-bitfield-p "resect_field_is_bitfield") :boolean
  (record declaration))
(cffi:defcfun (field-width "resect_field_get_width") :long-long
  (record declaration))

;;;
;;; STRUCT
;;;
(cffi:defcfun (struct-fields "resect_struct_fields") collection
  (struct declaration))

;;;
;;; UNION
;;;
(cffi:defcfun (union-fields "resect_union_fields") collection
  (struct declaration))

;;;
;;; ENUM
;;;
(cffi:defcfun (enum-constant-value "resect_enum_constant_value") :long-long
  (enum-constant declaration))
(cffi:defcfun (enum-constants "resect_enum_constants") collection
  (enum declaration))

;;;
;;; FUNCTION
;;;
(cffi:defcfun (function-parameters "resect_function_parameters") collection
  (function declaration))
(cffi:defcfun (function-return-type "resect_function_get_return_type") type
  (function declaration))
(cffi:defcfun (function-variadic-p "resect_function_is_variadic") :boolean
  (function declaration))
(cffi:defcfun (function-storage-class "resect_function_get_storage_class") storage-class
  (function declaration))
(cffi:defcfun (function-calling-convention "resect_function_get_calling_convention") calling-convention
  (function declaration))


;;;
;;; TYPEDEF
;;;
(cffi:defcfun (typedef-aliased-type "resect_typedef_get_aliased_type") type
  (typedef declaration))

;;;
;;; PARSING
;;;
(cffi:defcfun (make-options "resect_options_create") options)
(cffi:defcfun (options-add-include-path "resect_options_add_include_path") :void
  (opts options)
  (path :string))
(cffi:defcfun (options-add-framework-path "resect_options_add_framework_path") :void
  (opts options)
  (path :string))
(cffi:defcfun (options-add-language "resect_options_add_language") :void
  (opts options)
  (language :string))
(cffi:defcfun (options-add-standard "resect_options_add_standard") :void
  (opts options)
  (standard :string))
(cffi:defcfun (options-add-target "resect_options_add_target") :void
  "<arch><sub>-<vendor>-<sys>-<abi>"
  (opts options)
  (target :string))
(cffi:defcfun (destroy-options "resect_options_free") :void
  (opts options))


(cffi:defcfun (parse "resect_parse") translation-unit
  (filename :string)
  (opts options))


(cffi:defcfun (free "resect_free") :void
  (unit translation-unit))
