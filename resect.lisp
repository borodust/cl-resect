(cl:defpackage :%resect
  (:use)
  (:export #:parse
           #:free

           #:collection-iterator
           #:iterator-next
           #:iterator-value
           #:iterator-free

           #:translation-unit-declarations
           #:translation-unit-language

           #:declaration-kind
           #:declaration-id
           #:declaration-name
           #:declaration-mangled-name
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

           #:reference-pointee-type
           #:reference-lvalue-p

           #:typedef-aliased-type

           #:enum-constants
           #:enum-type
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
           #:class-fields
           #:class-methods

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
  (:constructor 11)
  (:destructor 12)
  (:converter 13)
  (:type-reference 14)
  (:template-reference 15)
  (:enum-constant 16)
  (:macro 17))


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


(cffi:defcenum language
  (:unknown 0)
  (:c 1)
  (:c++ 2)
  (:obj-c 3))


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
(cffi:defcfun ("resect_collection_iterator" collection-iterator) iterator
  (collection collection))
(cffi:defcfun ("resect_iterator_next" iterator-next) :boolean
  (iterator iterator))
(cffi:defcfun ("resect_iterator_value" iterator-value) :pointer
  (iterator iterator))
(cffi:defcfun ("resect_iterator_free" iterator-free) :void
  (iterator iterator))

;;;
;;; LOCATION
;;;
(cffi:defcfun ("resect_location_line" location-line) :unsigned-int
  (location location))
(cffi:defcfun ("resect_location_column" location-column) :unsigned-int
  (location location))
(cffi:defcfun ("resect_location_name" location-name) :string
  (location location))

;;;
;;; TYPE
;;;
(cffi:defcfun ("resect_type_get_kind" type-kind) type-kind
  (type type))
(cffi:defcfun ("resect_type_get_category" type-category) type-category
  (type type))
(cffi:defcfun ("resect_type_get_name" type-name) :string
  (type type))
(cffi:defcfun ("resect_type_sizeof" type-size) :long-long
  (type type))
(cffi:defcfun ("resect_type_alignof" type-alignment) :long-long
  (type type))
(cffi:defcfun ("resect_type_offsetof" type-field-offset) :long-long
  (type type)
  (field :string))
(cffi:defcfun ("resect_type_get_declaration" type-declaration) declaration
  (type type))

;;;
;;; ARRAY
;;;
(cffi:defcfun ("resect_array_get_size" array-size) :long-long
  (type type))
(cffi:defcfun ("resect_array_get_element_type" array-element-type) type
  (type type))

;;;
;;; POINTER
;;;
(cffi:defcfun ("resect_pointer_get_pointee_type" pointer-pointee-type) type
  (type type))


;;;
;;; POINTER
;;;
(cffi:defcfun ("resect_reference_get_pointee_type" reference-pointee-type) type
  (type type))
(cffi:defcfun ("resect_reference_is_lvalue" reference-lvalue-p) :boolean
  (type type))


;;;
;;; DECLARATION
;;;
(cffi:defcfun ("resect_decl_get_kind" declaration-kind) declaration-kind
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_id" declaration-id) :string
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_location" declaration-location) location
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_name" declaration-name) :string
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_mangled_name" declaration-mangled-name) :string
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_comment" declaration-comment) :string
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_type" declaration-type) type
  (declaration declaration))


;;;
;;; UNIT
;;;
(cffi:defcfun ("resect_unit_declarations" translation-unit-declarations) collection
  (unit translation-unit))

(cffi:defcfun ("resect_unit_get_language" translation-unit-language) language
  (unit translation-unit))


(cffi:defcfun ("resect_field_get_offset" field-offset) :long-long
  (record declaration))
(cffi:defcfun ("resect_field_is_bitfield" field-bitfield-p) :boolean
  (record declaration))
(cffi:defcfun ("resect_field_get_width" field-width) :long-long
  (record declaration))

;;;
;;; STRUCT
;;;
(cffi:defcfun ("resect_struct_fields" struct-fields) collection
  (struct declaration))

;;;
;;; UNION
;;;
(cffi:defcfun ("resect_union_fields" union-fields) collection
  (struct declaration))


;;;
;;; CLASS
;;;
(cffi:defcfun ("resect_class_fields" class-fields) collection
  (struct declaration))


(cffi:defcfun ("resect_class_methods" class-methods) collection
  (struct declaration))


;;;
;;; ENUM
;;;
(cffi:defcfun ("resect_enum_constant_value" enum-constant-value) :long-long
  (enum-constant declaration))
(cffi:defcfun ("resect_enum_constants" enum-constants) collection
  (enum declaration))
(cffi:defcfun ("resect_enum_get_type" enum-type) type
  (enum declaration))

;;;
;;; FUNCTION
;;;
(cffi:defcfun ("resect_function_parameters" function-parameters) collection
  (function declaration))
(cffi:defcfun ("resect_function_get_return_type" function-return-type) type
  (function declaration))
(cffi:defcfun ("resect_function_is_variadic" function-variadic-p) :boolean
  (function declaration))
(cffi:defcfun ("resect_function_get_storage_class" function-storage-class) storage-class
  (function declaration))
(cffi:defcfun ("resect_function_get_calling_convention" function-calling-convention) calling-convention
  (function declaration))


;;;
;;; TYPEDEF
;;;
(cffi:defcfun ("resect_typedef_get_aliased_type" typedef-aliased-type) type
  (typedef declaration))

;;;
;;; PARSING
;;;
(cffi:defcfun ("resect_options_create" make-options) options)
(cffi:defcfun ("resect_options_add_include_path" options-add-include-path) :void
  (opts options)
  (path :string))
(cffi:defcfun ("resect_options_add_framework_path" options-add-framework-path) :void
  (opts options)
  (path :string))
(cffi:defcfun ("resect_options_add_language" options-add-language) :void
  (opts options)
  (language :string))
(cffi:defcfun ("resect_options_add_standard" options-add-standard) :void
  (opts options)
  (standard :string))
(cffi:defcfun ("resect_options_add_target" options-add-target) :void
  "<arch><sub>-<vendor>-<sys>-<abi>"
  (opts options)
  (target :string))
(cffi:defcfun ("resect_options_free" destroy-options) :void
  (opts options))


(cffi:defcfun ("resect_parse" parse) translation-unit
  (filename :string)
  (opts options))


(cffi:defcfun ("resect_free" free) :void
  (unit translation-unit))
