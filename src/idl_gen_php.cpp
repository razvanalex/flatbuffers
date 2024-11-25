/*
 * Copyright 2014 Google Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// independent from idl_parser, since this code is not needed for most clients

#include "idl_gen_php.h"

#include <string>

#include "codegen/idl_namer.h"
#include "flatbuffers/code_generators.h"
#include "flatbuffers/flatbuffers.h"
#include "flatbuffers/idl.h"
#include "flatbuffers/reflection.h"
#include "flatbuffers/util.h"

namespace flatbuffers {

static Namer::Config PhpDefaultConfig() {
  return { /*types=*/Case::kKeep,
           /*constants=*/Case::kKeep,
           /*methods=*/Case::kLowerCamel,
           /*functions=*/Case::kUpperCamel,
           /*fields=*/Case::kKeep,
           /*variables=*/Case::kLowerCamel,
           /*variants=*/Case::kKeep,
           /*enum_variant_seperator=*/"::",
           /*escape_keywords=*/Namer::Config::Escape::BeforeConvertingCase,
           /*namespaces=*/Case::kKeep,
           /*namespace_seperator=*/"\\",
           /*object_prefix=*/"",
           /*object_suffix=*/"T",
           /*keyword_prefix=*/"",
           /*keyword_suffix=*/"_",
           /*keywords_casing=*/Namer::Config::KeywordsCasing::CASE_INSENSITIVE,
           /*filenames=*/Case::kKeep,
           /*directories=*/Case::kKeep,
           /*output_path=*/"",
           /*filename_suffix=*/"_generated",
           /*filename_extension=*/".php" };
}

static std::set<std::string> PhpKeywords() {
  // List of keywords retrieved from here:
  // https://www.php.net/manual/en/reserved.keywords.php
  //
  // List of reserved words retrieved from here:
  // https://www.php.net/manual/en/reserved.other-reserved-words.php
  return { "__halt_compiler",
           "abstract",
           "and",
           "array",
           "as",
           "break",
           "callable",
           "case",
           "catch",
           "class",
           "clone",
           "const",
           "continue",
           "declare",
           "default",
           "die",
           "do",
           "echo",
           "else",
           "elseif",
           "empty",
           "enddeclare",
           "endfor",
           "endforeach",
           "endif",
           "endswitch",
           "endwhile",
           "eval",
           "exit",
           "extends",
           "final",
           "for",
           "foreach",
           "function",
           "global",
           "goto",
           "if",
           "implements",
           "include",
           "include_once",
           "instanceof",
           "insteadof",
           "interface",
           "isset",
           "list",
           "namespace",
           "new",
           "or",
           "print",
           "private",
           "protected",
           "public",
           "require",
           "require_once",
           "return",
           "static",
           "switch",
           "throw",
           "trait",
           "try",
           "unset",
           "use",
           "var",
           "while",
           "xor",
           "int",
           "float",
           "bool",
           "string",
           "true",
           "false",
           "null",
           "void",
           "iterable",
           "object",
           "mixed",
           "never",
           "enum",
           "resource",
           "numeric" };
}

namespace php {
// Hardcode spaces per indentation.
const std::string Indent = "    ";
class PhpGenerator : public BaseGenerator {
 public:
  PhpGenerator(const Parser &parser, const std::string &path,
               const std::string &file_name)
      : BaseGenerator(parser, path, file_name, "\\", "\\", "php"),
        float_const_gen_("NAN", "INF", "-INF"),
        namer_(WithFlagOptions(PhpDefaultConfig(), parser.opts, path),
               PhpKeywords()) {}
  bool generate() {
    if (!GenerateEnums()) return false;
    if (!GenerateStructs()) return false;
    return true;
  }

 private:
  bool GenerateEnums() {
    for (auto it = parser_.enums_.vec.begin(); it != parser_.enums_.vec.end();
         ++it) {
      auto &enum_def = **it;
      std::string enumcode;
      GenEnum(enum_def, &enumcode);
      if (!SaveType(enum_def, enumcode, false)) return false;
    }
    return true;
  }

  bool GenerateStructs() {
    for (auto it = parser_.structs_.vec.begin();
         it != parser_.structs_.vec.end(); ++it) {
      auto &struct_def = **it;
      std::string declcode;
      GenStruct(struct_def, &declcode);
      if (!SaveType(struct_def, declcode, true)) return false;
    }
    return true;
  }

  void GenNativeUnpack(const StructDef &struct_def, std::string *code_ptr) {
    if (struct_def.generated) return;

    std::string &code = *code_ptr;
    const std::string object_type = namer_.ObjectType(struct_def);

    code += "\n";
    code += Indent + "/**\n";
    code += Indent + " * @param " + object_type + " $o\n";
    code += Indent + " */\n";
    code += Indent + "public function unPackTo(&$o)\n";
    code += Indent + "{\n";

    for (auto it = struct_def.fields.vec.begin();
         it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (field.deprecated) continue;
      if (IsScalar(field.value.type.base_type) && IsUnion(field.value.type))
        continue;

      const std::string field_field = namer_.Field(field);
      const std::string field_var = namer_.Variable(field);
      const std::string length_var = "$" + field_var + "Length";

      switch (field.value.type.base_type) {
        case BASE_TYPE_STRUCT: {
          GenUnPackForStruct(struct_def, field, &code);
          break;
        }
        case BASE_TYPE_UNION: {
          GenUnPackForUnion(struct_def, field, &code);
          break;
        }
        case BASE_TYPE_ARRAY:
        case BASE_TYPE_VECTOR: {
          auto vectortype = field.value.type.VectorType();
          if (vectortype.base_type == BASE_TYPE_STRUCT) {
            GenUnPackForStructVector(struct_def, field, &code);
          } else if (vectortype.base_type == BASE_TYPE_UCHAR) {
            GenUnPackForUBytesField(struct_def, field, &code);
          } else {
            GenUnPackForScalarVector(struct_def, field, &code);
          }
          break;
        }
        default: GenUnPackForScalarOrString(struct_def, field, &code);
      }
    }
    code += Indent + "}\n";

    code += "\n";
    code += Indent + "/**\n";
    code += Indent + " * @return " + object_type + "\n";
    code += Indent + " */\n";
    code += Indent + "public function unPack()\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$o = new " + object_type + "();\n";
    code += Indent + Indent + "$this->unPackTo($o);\n";
    code += Indent + Indent + "return $o;\n";
    code += Indent + "}\n";
  }

  void GenUnPackForStruct(const StructDef &struct_def, const FieldDef &field,
                          std::string *code_ptr) const {
    auto &code = *code_ptr;

    const auto field_field = namer_.Field(field);
    const auto field_var = "$" + field_field;
    const auto field_obj = "$o->" + field_field;
    const auto field_value = "$this->" + GetMethod(struct_def, field) + "()";

    code += Indent + Indent + field_var + " = " + field_value + ";\n";
    code += Indent + Indent + "if (" + field_var + " !== null) {\n";
    code += Indent + Indent + Indent + field_obj + " = " + field_var +
            "->unPack();\n";
    code += Indent + Indent + "}\n";
  }

  void GenUnPackForUnion(const StructDef &struct_def, const FieldDef &field,
                         std::string *code_ptr) const {
    auto &code = *code_ptr;

    const auto field_field = namer_.Field(field);
    const auto field_type = namer_.Type(*field.value.type.enum_def);
    const auto field_var = "$" + field_field;
    const auto field_obj = "$o->" + field_field;
    const auto accessor = GetMethod(struct_def, field);

    code += Indent + Indent + field_var + " = $this->" + accessor + "Type();\n";
    code += Indent + Indent + "if (" + field_var + " !== null) {\n";
    code += Indent + Indent + Indent + field_obj + " = " + field_type +
            "::unPack(" + field_var + ", array($this, '" + accessor + "'));\n";
    code += Indent + Indent + "}\n";
  }

  void GenUnPackForStructVector(const StructDef &struct_def,
                                const FieldDef &field,
                                std::string *code_ptr) const {
    auto &code = *code_ptr;

    const auto field_field = namer_.Field(field);
    const auto array_field_val = "$o->" + field_field;
    const auto field_value_i =
        "$this->" + GetMethod(struct_def, field) + "($i)";
    const auto array_length = "$" + field_field + "_len";

    code += Indent + Indent + array_field_val + " = array();\n";
    code += Indent + Indent + array_length + " = $this->" +
            namer_.Method("get", field, "Length") + "();\n";
    code +=
        Indent + Indent + "for ($i = 0; $i < " + array_length + "; $i++) {\n";
    code += Indent + Indent + Indent + "array_push(" + array_field_val + ", " +
            field_value_i + "->unpack());\n";
    code += Indent + Indent + "}\n";
  }

  void GenUnPackForUBytesField(const StructDef &struct_def,
                               const FieldDef &field,
                               std::string *code_ptr) const {
    auto &code = *code_ptr;

    const auto array_field_val = "$o->" + namer_.Field(field);
    const auto field_value =
        "$this->" + GetMethod(struct_def, field) + "Bytes()";

    code += Indent + Indent + array_field_val + " = " + field_value + ";\n";
  }

  void GenUnPackForScalarVector(const StructDef &struct_def,
                                const FieldDef &field,
                                std::string *code_ptr) const {
    auto &code = *code_ptr;

    const auto field_field = namer_.Field(field);
    const auto array_field_val = "$o->" + field_field;
    const auto field_value_i =
        "$this->" + GetMethod(struct_def, field) + "($i)";
    const auto array_length = "$" + field_field + "_len";

    code += Indent + Indent + array_field_val + " = array();\n";
    code += Indent + Indent + array_length + " = $this->" +
            namer_.Method("get", field, "Length") + "();\n";
    code +=
        Indent + Indent + "for ($i = 0; $i < " + array_length + "; $i++) {\n";
    code += Indent + Indent + Indent + "array_push(" + array_field_val + ", " +
            field_value_i + ");\n";
    code += Indent + Indent + "}\n";
  }

  void GenUnPackForScalarOrString(const StructDef &struct_def,
                                  const FieldDef &field,
                                  std::string *code_ptr) const {
    auto &code = *code_ptr;

    const auto field_field = namer_.Field(field);
    const auto field_obj = "$o->" + field_field;
    const auto field_value = "$this->" + GetMethod(struct_def, field) + "()";

    code += Indent + Indent + field_obj + " = " + field_value + ";\n";
  }

  void GenNativeStruct(const StructDef &struct_def, std::string *code_ptr) {
    if (struct_def.generated) return;

    BeginClassForObjectAPI(struct_def, code_ptr);
    GenConstructorForObjectAPI(struct_def, code_ptr);
    if (!struct_def.fixed) {
      GenPackForTable(struct_def, code_ptr);
    } else {
      GenPackForStruct(struct_def, code_ptr);
    }
    EndClass(code_ptr);

    std::string &code = *code_ptr;
    code += "\n";
  }

  void GenConstructorForObjectAPI(const StructDef &struct_def,
                                  std::string *code_ptr) {
    std::string &code = *code_ptr;
    std::string fields = "";
    std::string php_doc =
        (struct_def.fields.vec.empty() ? "" : Indent + "/**\n");
    std::string constructor_impl = Indent + "{\n";
    std::string constructor_decl = Indent + "public function __construct(";
    int num_args = 0;

    for (auto it = struct_def.fields.vec.begin();
         it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (field.deprecated) continue;
      if (IsScalar(field.value.type.base_type) && IsUnion(field.value.type))
        continue;

      const auto field_field = namer_.Field(field);
      const auto field_var = "$" + field_field;
      const auto field_this = "$this->" + field_field;

      std::string field_type;
      if (IsStruct(field.value.type) || IsTable(field.value.type)) {
        field_type = namer_.ObjectType(GenTypeGet(field.value.type));
      } else if (IsUnion(field.value.type)) {
        field_type = namer_.ObjectType(*field.value.type.enum_def);
      } else if (IsArray(field.value.type) || IsVector(field.value.type)) {
        field_type = "array";
      } else {
        field_type = GenTypeGet(field.value.type);
      }

      fields += Indent + "/**\n";
      fields += Indent + " * @var " + field_type + " " + field_var + "\n";
      fields += Indent + " */\n";
      fields += Indent + "public " + field_var + ";\n\n";

      php_doc += Indent + " * @param " + field_type + " " + field_var + "\n";
      // FIXME: linter is dumb... reformat all on-line ifs...
      if (num_args++ > 0) { constructor_decl += ", "; }

      constructor_decl += field_var + " = ";
      if (IsUnion(field.value.type)) {
        constructor_decl += "null";
      } else if (IsArray(field.value.type)) {
        constructor_decl += "array()";
      } else if (IsVector(field.value.type) && field.value.type.VectorType().base_type == BASE_TYPE_UCHAR) {
        constructor_decl += "\"\"";
      } else if (IsVector(field.value.type)) {
        constructor_decl += "array()";
      } else {
        constructor_decl += GenDefaultValue(field);
      }

      constructor_impl +=
          Indent + Indent + field_this + " = " + field_var + ";\n";
    }

    php_doc += struct_def.fields.vec.empty() ? "" : Indent + " */\n";
    constructor_decl += ")\n";
    constructor_impl += Indent + "}\n";

    code += fields;
    code += php_doc;
    code += constructor_decl;
    code += constructor_impl;
    code += "\n";
  }

  void BeginClassForObjectAPI(const StructDef &struct_def,
                              std::string *code_ptr) {
    auto &code = *code_ptr;
    code += "class " + namer_.ObjectType(struct_def) +
            " implements IGeneratedObject\n";
    code += "{\n";
  }

  void GenPackForStructField(const StructDef &struct_def, const FieldDef &field,
                             std::string *code_prefix_ptr,
                             std::string *code_ptr) const {
    auto &code_prefix = *code_prefix_ptr;
    auto &code = *code_ptr;
    const auto field_field = namer_.Field(field);
    const auto field_var = "$" + field_field;
    const auto field_this = "$this->" + field_field;
    const auto struct_type = namer_.Type(struct_def);
    const auto add_field_method = namer_.Method("add", field);

    if (field.value.type.struct_def->fixed) {
      // Pure struct fields need to be created along with their parent
      // structs.
      code += Indent + Indent + "if (" + field_this + " !== null) {\n";
      code += Indent + Indent + Indent + field_var + " = " + field_this +
              "->pack($builder);\n";
      code += Indent + Indent + Indent + struct_type + "::" + add_field_method +
              "($builder, " + field_var + ");\n";
      code += Indent + Indent + "}\n";
    } else {
      // Tables need to be created before their parent structs are created.
      code_prefix += Indent + Indent + "if (" + field_this + " !== null) {\n";
      code_prefix += Indent + Indent + Indent + field_var + " = " + field_this +
                     "->pack($builder);\n";
      code_prefix += Indent + Indent + "}\n";

      code += Indent + Indent + "if (" + field_this + " !== null) {\n";
      code += Indent + Indent + Indent + struct_type + "::" + add_field_method +
              "($builder, " + field_var + ");\n";
      code += Indent + Indent + "}\n";
    }
  }

  void GenPackForUnionField(const StructDef &struct_def, const FieldDef &field,
                            std::string *code_prefix_ptr,
                            std::string *code_ptr) const {
    auto &code_prefix = *code_prefix_ptr;
    auto &code = *code_ptr;
    const auto field_field = namer_.Field(field);
    const auto field_var = "$" + field_field;
    const auto field_this = "$this->" + field_field;
    const auto add_field_method = namer_.Method("add", field);
    const auto add_field_type_method =
        namer_.Method("add", field.name + UnionTypeFieldSuffix());
    const auto struct_type = namer_.Type(struct_def);

    code_prefix += Indent + Indent + "if (" + field_this + " !== null && " +
                   field_this + "->value !== null) {\n";
    code_prefix += Indent + Indent + Indent + field_var + " = " + field_this +
                   "->value->pack($builder);\n";
    code_prefix += Indent + Indent + "}\n";

    code += Indent + Indent + "if (" + field_this + " !== null && " +
            field_this + "->type !== null) {\n";
    code += Indent + Indent + Indent + struct_type +
            "::" + add_field_type_method + "($builder, " + field_this +
            "->type);\n";
    code += Indent + Indent + Indent + struct_type + "::" + add_field_method +
            "($builder, " + field_var + ");\n";
    code += Indent + Indent + "}\n";
  }

  void GenPackForStructVectorField(const StructDef &struct_def,
                                   const FieldDef &field,
                                   std::string *code_prefix_ptr,
                                   std::string *code_ptr) const {
    auto &code_prefix = *code_prefix_ptr;
    auto &code = *code_ptr;
    const auto field_field = namer_.Field(field);
    const auto field_var = "$" + field_field;
    const auto field_arr = field_var + "_arr";
    const auto field_this = "$this->" + field_field;
    const auto struct_type = namer_.Type(struct_def);
    const auto start_field_method = namer_.Method("start", field, "Vector");
    const auto create_field_method = namer_.Method("create", field, "Vector");
    const auto add_field_method = namer_.Method("add", field);

    // Creates the field.
    code_prefix += Indent + Indent + "if (" + field_this + " !== null) {\n";
    if (field.value.type.struct_def->fixed) {
      // FIXME(razvanalex): Need to check this
      code_prefix += Indent + Indent + Indent;
      code_prefix += struct_type + "::" + start_field_method +
                     "($builder, count(" + field_this + "));\n";
      code_prefix += Indent + Indent + Indent;
      code_prefix +=
          "for ($i = count(" + field_this + ") - 1; $i >= 0; $i--) {\n";
      code_prefix += Indent + Indent + Indent + Indent;
      code_prefix += field_this + "[$i]->pack($builder);\n";
      code_prefix += Indent + Indent + Indent;
      code_prefix += "}\n";
      code_prefix += Indent + Indent + Indent;
      code_prefix += field_var + " = $builder->endVector();\n";
    } else {
      // If the vector is a struct vector, we need to first build accessor for
      // each struct element.
      code_prefix += Indent + Indent + Indent;
      code_prefix += field_arr + " = array();\n";
      code_prefix += Indent + Indent + Indent;
      code_prefix += "for ($i = 0; $i < count(" + field_this + "); $i++) {\n";
      code_prefix += Indent + Indent + Indent + Indent;
      code_prefix += "array_push(" + field_arr + ", " + field_this +
                     "[i]->pack($builder));\n";
      code_prefix += Indent + Indent + Indent;
      code_prefix += "}\n";
      code_prefix += Indent + Indent + Indent;
      code_prefix += field_var + " = " + struct_type +
                     "::" + create_field_method + "($builder, " + field_arr +
                     ");\n";
    }
    code_prefix += Indent + Indent;
    code_prefix += "}\n";

    // Adds the field into the struct.
    code += Indent + Indent;
    code += "if (" + field_this + " !== null) {\n";
    code += Indent + Indent + Indent;
    code += struct_type + "::" + add_field_method + "($builder, " + field_var +
            ");\n";
    code += Indent + Indent + "}\n";
  }

  void GenPackForScalarVectorFieldHelper(const StructDef &struct_def,
                                         const FieldDef &field,
                                         std::string *code_ptr,
                                         int indents) const {
    auto &code = *code_ptr;
    const auto field_field = namer_.Field(field);
    const auto field_this = "$this->" + field_field;
    const auto start_field_method = namer_.Method("start", field, "Vector");
    const auto struct_type = namer_.Type(struct_def);
    const auto vectortype = field.value.type.VectorType();
    const auto indent_outter = std::string(indents * Indent.length(), ' ');
    const auto indent_inner = std::string((indents + 1) * Indent.length(), ' ');

    code += indent_outter + struct_type + "::" + start_field_method +
            "($builder, count(" + field_this + "));\n";
    code += indent_outter + "for ($i = count(" + field_this +
            ") - 1; $i >= 0; $i--) {\n";

    std::string type_name;
    switch (vectortype.base_type) {
      case BASE_TYPE_BOOL: type_name = "Bool"; break;
      case BASE_TYPE_CHAR: type_name = "Sbyte"; break;
      case BASE_TYPE_UTYPE:
      case BASE_TYPE_UCHAR: type_name = "Byte"; break;
      case BASE_TYPE_SHORT: type_name = "Shot"; break;
      case BASE_TYPE_USHORT: type_name = "Ushort"; break;
      case BASE_TYPE_INT: type_name = "Int"; break;
      case BASE_TYPE_UINT: type_name = "Uint"; break;
      case BASE_TYPE_LONG: type_name = "Long"; break;
      case BASE_TYPE_ULONG: type_name = "Ulong"; break;
      case BASE_TYPE_FLOAT: type_name = "Float"; break;
      case BASE_TYPE_DOUBLE: type_name = "Double"; break;
      default: type_name = "Offset"; break;
    }

    code += indent_inner + "$builder->" + namer_.Method("add", type_name);
  }

  void GenPackForUBytesField(const StructDef &struct_def, const FieldDef &field,
                             std::string *code_prefix_ptr,
                             std::string *code_ptr) const {
    auto &code = *code_ptr;
    auto &code_prefix = *code_prefix_ptr;
    const auto field_field = namer_.Field(field);
    const auto field_var = "$" + field_field;
    const auto field_this = "$this->" + field_field;
    const auto add_field_method = namer_.Method("add", field);
    const auto struct_type = namer_.Type(struct_def);

    code_prefix += Indent + Indent + "if (" + field_this + " !== null) {\n";
    code_prefix += Indent + Indent + Indent + field_var +
                   " = $builder->createBytesVector(" + field_this + ");\n";
    code_prefix += Indent + Indent + "}\n";

    code += Indent + Indent + "if (" + field_this + " !== null) {\n";
    code += Indent + Indent + Indent;
    code += struct_type + "::" + add_field_method + "($builder, " + field_var +
            ");\n";
    code += Indent + Indent + "}\n";
  }

  void GenPackForScalarVectorField(const StructDef &struct_def,
                                   const FieldDef &field,
                                   std::string *code_prefix_ptr,
                                   std::string *code_ptr) const {
    auto &code = *code_ptr;
    auto &code_prefix = *code_prefix_ptr;
    const auto field_field = namer_.Field(field);
    const auto field_var = "$" + field_field;
    const auto field_arr = field_var + "_arr";
    const auto field_this = "$this->" + field_field;
    const auto add_field_method = namer_.Method("add", field);
    const auto create_field_method = namer_.Method("create", field, "Vector");
    const auto struct_type = namer_.Type(struct_def);

    // Adds the field into the struct.
    code += Indent + Indent + "if (" + field_this + " !== null) {\n";
    code += Indent + Indent + Indent;
    code += struct_type + "::" + add_field_method + "($builder, " + field_var +
            ");\n";
    code += Indent + Indent + "}\n";

    // Creates the field.
    code_prefix += Indent + Indent + "if (" + field_this + " !== null) {\n";
    // If the vector is a string vector, we need to first build accessor for
    // each string element. And this generated code, needs to be
    // placed ahead of code_prefix.
    auto vectortype = field.value.type.VectorType();
    if (IsString(vectortype)) {
      code_prefix += Indent + Indent + Indent + field_arr + " = array();\n";
      code_prefix += Indent + Indent + Indent + "for ($i = 0; $i < count(" +
                     field_this + "); $i++) {\n";
      code_prefix += Indent + Indent + Indent + Indent + "array_push(" +
                     field_arr + ", $builder->createString(" + field_this +
                     "[$i]));\n";
      code_prefix += Indent + Indent + Indent + "}\n";
      code_prefix += Indent + Indent + Indent + field_var + " = " +
                     struct_type + "::" + create_field_method + "($builder, " +
                     field_arr + ");\n";
      code_prefix += Indent + Indent + "}\n";
      return;
    }

    code_prefix += Indent + Indent + Indent + field_var + " = " + struct_type +
                   "::" + create_field_method + "($builder, " + field_this +
                   ");\n";
    code_prefix += Indent + Indent + "}\n";
  }

  void GenPackForTable(const StructDef &struct_def, std::string *code_ptr) {
    std::string &code_base = *code_ptr;
    std::string code, code_prefix;
    const auto struct_var = "$" + namer_.Variable(struct_def);
    const auto struct_type = namer_.Type(struct_def);

    code_base += Indent + "/**\n";
    code_base += Indent + " * @param FlatBufferBuilder $builder\n";
    code_base += Indent + " * @return int offset\n";
    code_base += Indent + " */\n";
    code_base += Indent + "public function pack(FlatBufferBuilder $builder)\n";
    code_base += Indent + "{\n";

    code += Indent + Indent + struct_type + "::start" + struct_type +
            "($builder);\n";
    for (auto it = struct_def.fields.vec.begin();
         it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (field.deprecated) continue;
      if (IsScalar(field.value.type.base_type) && IsUnion(field.value.type))
        continue;

      const auto add_field_method = namer_.Method("add", field);
      const auto field_field = namer_.Field(field);
      const auto field_var = "$" + field_field;
      const auto field_this = "$this->" + field_field;

      switch (field.value.type.base_type) {
        case BASE_TYPE_STRUCT: {
          GenPackForStructField(struct_def, field, &code_prefix, &code);
          break;
        }
        case BASE_TYPE_UNION: {
          GenPackForUnionField(struct_def, field, &code_prefix, &code);
          break;
        }
        case BASE_TYPE_ARRAY:
        case BASE_TYPE_VECTOR: {
          auto vectortype = field.value.type.VectorType();
          if (vectortype.base_type == BASE_TYPE_STRUCT) {
            GenPackForStructVectorField(struct_def, field, &code_prefix, &code);
          } else if (vectortype.base_type == BASE_TYPE_UCHAR) {
            GenPackForUBytesField(struct_def, field, &code_prefix, &code);
          } else {
            GenPackForScalarVectorField(struct_def, field, &code_prefix, &code);
          }
          break;
        }
        case BASE_TYPE_STRING: {
          code_prefix +=
              Indent + Indent + "if (" + field_this + " !== null) {\n";
          code_prefix += Indent + Indent + Indent + field_var +
                         " = $builder->createString(" + field_this + ");\n";
          code_prefix += Indent + Indent + "}\n";
          code += Indent + Indent + "if (" + field_this + " !== null) {\n";
          code += Indent + Indent + Indent + struct_type +
                  "::" + add_field_method + "($builder, " + field_var + ");\n";
          code += Indent + Indent + "}\n";
          break;
        }
        default:
          // Generates code for scalar values. If the value equals to the
          // default value, builder will automatically ignore it. So we don't
          // need to check the value ahead.
          code += Indent + Indent + struct_type + "::" + add_field_method +
                  "($builder, " + field_this + ");\n";
          break;
      }
    }
    code += Indent + Indent + struct_var + " = " + struct_type +
            "::" + "end" + struct_type + "($builder);\n";
    code += Indent + Indent + "return " + struct_var + ";\n";
    code += Indent + "}\n";

    code_base += code_prefix + code;
  }

  void GenPackForStruct(const StructDef &struct_def, std::string *code_ptr) {
    std::string &code = *code_ptr;
    const auto struct_fn = namer_.Function(struct_def);
    const auto class_name = namer_.Type(struct_def);

    code += Indent + "/**\n";
    code += Indent + " * @param FlatBufferBuilder $builder\n";
    code += Indent + " * @return int offset\n";
    code += Indent + " */\n";
    code += Indent + "public function pack(FlatBufferBuilder $builder)\n";
    code += Indent + "{\n";
    code += Indent + Indent + "return " + class_name + "::create" + struct_fn +
            "($builder";
    StructBuilderArgs(struct_def, "this->", "->", code_ptr);
    code += ");\n";
    code += Indent + "}\n";
  }

  void GenNativeUnion(const EnumDef &enum_def, std::string *code_ptr) {
    std::string &code = *code_ptr;

    GenNativeUnionClass(enum_def, code_ptr);
    GenNativeUnionPack(enum_def, code_ptr);
    EndClass(code_ptr);
    code += "\n";
  }

  void GenNativeUnionClass(const EnumDef &enum_def, std::string *code_ptr) {
    std::string &code = *code_ptr;
    const std::string type = namer_.Type(enum_def);
    const std::string union_type = namer_.ObjectType(enum_def);

    code += "class " + union_type + "\n";
    code += "{\n";
    code += Indent + "/**\n";
    code += Indent + " * @var " + type + " $type\n";
    code += Indent + " */\n";
    code += Indent + "public $type;\n";
    code += "\n";
    code += Indent + "/**\n";
    code += Indent + " * @var mixed $value\n";
    code += Indent + " */\n";
    code += Indent + "public $value;\n";
    code += "\n";
    code += Indent + "/**\n";
    code += Indent + " * @param " + type + " $type\n";
    code += Indent + " * @param mixed $value\n";
    code += Indent + " */\n";
    code += Indent + "public function __construct($type, $value)\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$this->type = $type;\n";
    code += Indent + Indent + "$this->value = $value;\n";
    code += Indent + "}\n";
    code += "\n";
  }

  void GenNativeUnionPack(const EnumDef &enum_def, std::string *code_ptr) {
    std::string &code = *code_ptr;
    const std::string type = namer_.Type(enum_def);
    const std::string union_type = namer_.ObjectType(enum_def);

    code += Indent + "/**\n";
    code += Indent + " * @param FlatBufferBuilder $builder\n";
    code += Indent + " * @return int offset\n";
    code += Indent + " */\n";
    code += Indent + "public function pack(FlatBufferBuilder $builder)\n";
    code += Indent + "{\n";
    code += Indent + Indent + "switch ($this->type) {\n";
    for (auto it2 = enum_def.Vals().begin(); it2 != enum_def.Vals().end();
         ++it2) {
      const EnumVal &ev = **it2;
      if (ev.IsZero()) continue;
      code += Indent + Indent + Indent + "case " +
              namer_.EnumVariant(enum_def, ev) + ":\n";
      if (ev.union_type.base_type == BASE_TYPE_STRUCT) {
        code += Indent + Indent + Indent + Indent +
                "return $this->value->pack($builder);\n";
      } else {
        code += Indent + Indent + Indent + Indent + "return $this->value" +
                std::to_string(ev.union_type.base_type) + ";\n";
      }
    }
    code += Indent + Indent + Indent + "default:\n";
    code += Indent + Indent + Indent + Indent + "return 0;\n";
    code += Indent + Indent + "}\n";
    code += Indent + "}\n";
  }

  void GenNativeUnionUnPack(const EnumDef &enum_def, std::string *code_ptr) {
    std::string &code = *code_ptr;
    const std::string type = namer_.Type(enum_def);
    const std::string union_type = namer_.ObjectType(enum_def);

    code += "\n";
    code += Indent + "/**\n";
    code += Indent + " * @return " + union_type + "\n";
    code += Indent + " */\n";
    code += Indent + "public static function unPack($union_type, $accessor)\n";
    code += Indent + "{\n";
    code += Indent + Indent + "switch ($union_type) {\n";

    for (auto it2 = enum_def.Vals().begin(); it2 != enum_def.Vals().end();
         ++it2) {
      const EnumVal &ev = **it2;
      if (ev.IsZero()) continue;

      const auto enum_variant = namer_.EnumVariant(enum_def, ev);

      std::string enum_type;
      switch (ev.union_type.base_type) {
        case BASE_TYPE_STRUCT:
          enum_type = "new " + ModuleFor(ev.union_type.struct_def) + "()";
          break;
        default: break;
      }

      code += Indent + Indent + Indent + "case " + enum_variant + ":\n";
      code += Indent + Indent + Indent + Indent + "$obj = $accessor(" +
              enum_type + ");\n";
      code += Indent + Indent + Indent + Indent + "return new " + union_type +
              "($union_type, $obj->unPack());\n";
    }

    code += Indent + Indent + Indent + "default:\n";
    code += Indent + Indent + Indent + Indent + "return null;\n";
    code += Indent + Indent + "}\n";
    code += Indent + "}\n";
  }

  template<typename T> std::string ModuleFor(const T *def) const {
    // TODO: cleanup this mess
    if (!parser_.opts.one_file) { return "\\" + namer_.NamespacedType(*def); }

    std::string filename =
        StripExtension(def->file) + parser_.opts.filename_suffix;
    if (parser_.file_being_parsed_ == def->file) {
      return "\\" + StripPath(filename);  // make it a "local" import
    }

    std::string module = parser_.opts.include_prefix + filename;
    std::replace(module.begin(), module.end(), '/', '\\');
    return "\\" + module;
  }

  // Begin by declaring namespace and imports.
  void BeginFile(const std::string &name_space_name, const bool needs_imports,
                 std::string *code_ptr) {
    auto &code = *code_ptr;
    code += "<?php\n";
    code = code + "// " + FlatBuffersGeneratedWarning() + "\n\n";

    if (!name_space_name.empty()) {
      code += "namespace " + name_space_name + ";\n\n";
    }

    if (needs_imports) {
      code += "use \\Google\\FlatBuffers\\Struct;\n";
      code += "use \\Google\\FlatBuffers\\Table;\n";
      code += "use \\Google\\FlatBuffers\\ByteBuffer;\n";
      code += "use \\Google\\FlatBuffers\\FlatBufferBuilder;\n";
      code += "use \\Google\\FlatBuffers\\Constants;\n";
      code += "use \\Google\\FlatBuffers\\IUnpackableObject;\n";
      code += "use \\Google\\FlatBuffers\\IGeneratedObject;\n";
      code += "\n";
    }
  }

  // Save out the generated code for a Php Table type.
  bool SaveType(const Definition &def, const std::string &classcode,
                bool needs_imports) {
    if (!classcode.length()) return true;

    std::string code = "";
    BeginFile(FullNamespace("\\", *def.defined_namespace), needs_imports,
              &code);
    code += classcode;

    std::string filename =
        NamespaceDir(*def.defined_namespace) + def.name + ".php";
    return SaveFile(filename.c_str(), code, false);
  }

  // Begin a class declaration.
  void BeginClass(const StructDef &struct_def, std::string *code_ptr) {
    std::string &code = *code_ptr;
    if (struct_def.fixed) {
      code += "class " + namer_.Type(struct_def) + " extends Struct";
    } else {
      code += "class " + namer_.Type(struct_def) + " extends Table";
    }
    if (parser_.opts.generate_object_based_api) {
      code += " implements IUnpackableObject";
    }
    code += "\n{\n";
  }

  void EndClass(std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += "}\n";
  }

  // Begin enum code with a class declaration.
  void BeginEnum(const std::string &class_name, std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += "class " + namer_.Type(class_name) + "\n{\n";
  }

  // A single enum member.
  void EnumMember(const EnumDef &enum_def, const EnumVal &ev,
                  std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += Indent + "const ";
    code += namer_.Variant(ev);
    code += " = ";
    code += enum_def.ToString(ev) + ";\n";
  }

  // End enum code.
  void EndEnum(std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += "}\n";
  }

  // Initialize a new struct or table from existing data.
  void NewRootTypeFromBuffer(const StructDef &struct_def, std::string *code_ptr,
                             bool size_prefixed) {
    std::string &code = *code_ptr;
    const std::string sizePrefixed(size_prefixed ? "SizePrefixed" : "");
    const std::string struct_type = namer_.Type(struct_def);

    code += Indent + "/**\n";
    code += Indent + " * @param ByteBuffer $bb\n";
    code += Indent + " * @return " + struct_type + "\n";
    code += Indent + " */\n";
    code += Indent + "public static function ";
    code += namer_.Method("get" + sizePrefixed + "RootAs", struct_type);
    code += "(ByteBuffer $bb)\n";
    code += Indent + "{\n";

    code += Indent + Indent + "$obj = new " + struct_type + "();\n";
    if (size_prefixed) {
      code += Indent + Indent;
      code += "$bb->setPosition($bb->getPosition() + Constants::SIZEOF_INT);\n";
    }
    code += Indent + Indent;
    code += "return $obj->init($bb->getInt($bb->getPosition())";
    code += " + $bb->getPosition(), $bb);\n";
    code += Indent + "}\n\n";
  }

  // Initialize an existing object with other data, to avoid an allocation.
  void InitializeExisting(const StructDef &struct_def, std::string *code_ptr) {
    std::string &code = *code_ptr;

    code += Indent + "/**\n";
    code += Indent + " * @param int $_i offset\n";
    code += Indent + " * @param ByteBuffer $_bb\n";
    code += Indent + " * @return " + namer_.Type(struct_def) + "\n";
    code += Indent + " **/\n";
    code += Indent + "public function init($_i, ByteBuffer $_bb)\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$this->bb_pos = $_i;\n";
    code += Indent + Indent + "$this->bb = $_bb;\n";
    code += Indent + Indent + "return $this;\n";
    code += Indent + "}\n\n";
  }

  // Get the length of a vector.
  void GetVectorLen(const FieldDef &field, std::string *code_ptr) {
    std::string &code = *code_ptr;

    code += Indent + "/**\n";
    code += Indent + " * @return int\n";
    code += Indent + " */\n";
    code += Indent + "public function " +
            namer_.Method("get", field, "Length") + "()\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$o = $this->__offset(";
    code += NumToString(field.value.offset) + ");\n";
    code += Indent + Indent;
    code += "return $o != 0 ? $this->__vector_len($o) : 0;\n";
    code += Indent + "}\n\n";
  }

  // Get a [ubyte] vector as a byte array.
  void GetUByte(const FieldDef &field, std::string *code_ptr) {
    std::string &code = *code_ptr;

    code += Indent + "/**\n";
    code += Indent + " * @return string\n";
    code += Indent + " */\n";
    code += Indent + "public function " +
            namer_.Method("get", field.name, "Bytes") + "()\n";
    code += Indent + "{\n";
    code += Indent + Indent + "return $this->__vector_as_bytes(";
    code += NumToString(field.value.offset) + ");\n";
    code += Indent + "}\n\n";
  }

  // Get the value of a struct's scalar.
  void GetScalarFieldOfStruct(const FieldDef &field, std::string *code_ptr) {
    std::string &code = *code_ptr;

    code += Indent + "/**\n";
    code += Indent + " * @return " + GenTypeGet(field.value.type) + "\n";
    code += Indent + " */\n";
    code += Indent + "public function " +
            namer_.LegacyPhpField(GenGetter(field.value.type), field) + "()\n";
    code += Indent + "{\n";

    code += Indent + Indent + "return ";
    code += "$this->bb->";
    code += namer_.Method("get", GenTypeGet(field.value.type));
    code += "($this->bb_pos + ";
    code += NumToString(field.value.offset) + ")";
    code += ";\n";

    code += Indent + "}\n\n";
  }

  // Get the value of a table's scalar.
  void GetScalarFieldOfTable(const FieldDef &field, std::string *code_ptr) {
    std::string &code = *code_ptr;
    std::string optional = field.IsOptional() ? "?" : "";

    code += Indent + "/**\n";
    code += Indent + " * @return " + optional + GenTypeBasic(field.value.type) +
            "\n";
    code += Indent + " */\n";
    code += Indent + "public function " + namer_.Method("get", field) + "()\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$o = $this->__offset(" +
            NumToString(field.value.offset) + ");\n" + Indent + Indent +
            "return $o != 0 ? ";
    code += "$this->bb->";
    code += namer_.Method("get", GenTypeGet(field.value.type)) +
            "($o + $this->bb_pos)";
    code += " : " + GenDefaultValue(field) + ";\n";
    code += Indent + "}\n\n";
  }

  // Get a struct by initializing an existing struct.
  // Specific to Struct.
  void GetStructFieldOfStruct(const FieldDef &field, std::string *code_ptr) {
    std::string &code = *code_ptr;

    code += Indent + "/**\n";
    code += Indent + " * @return " + GenTypeGet(field.value.type) + "\n";
    code += Indent + " */\n";
    code += Indent + "public function " + namer_.Method("get", field) + "()\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$obj = new " + GenTypeGet(field.value.type) +
            "();\n";
    code += Indent + Indent + "$obj->init($this->bb_pos + " +
            NumToString(field.value.offset) + ", $this->bb);\n";
    code += Indent + Indent + "return $obj;\n";
    code += Indent + "}\n\n";
  }

  // Get a struct by initializing an existing struct.
  // Specific to Table.
  void GetStructFieldOfTable(const FieldDef &field, std::string *code_ptr) {
    std::string &code = *code_ptr;

    code += Indent + "public function " + namer_.Method("get", field.name);
    code += "()\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$obj = new " +
            ModuleFor(field.value.type.struct_def) + "();\n";
    code += Indent + Indent + "$o = $this->__offset(" +
            NumToString(field.value.offset) + ");\n";
    code += Indent + Indent;
    code += "return $o != 0 ? $obj->init(";
    if (field.value.type.struct_def->fixed) {
      code += "$o + $this->bb_pos, $this->bb) : ";
    } else {
      code += "$this->__indirect($o + $this->bb_pos), $this->bb) : ";
    }
    code += GenDefaultValue(field) + ";\n";
    code += Indent + "}\n\n";
  }

  // Get the value of a string.
  void GetStringField(const FieldDef &field, std::string *code_ptr) {
    std::string &code = *code_ptr;

    code += Indent + "public function " + namer_.Method("get", field) + "()\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$o = $this->__offset(" +
            NumToString(field.value.offset) + ");\n";
    code += Indent + Indent;
    code += "return $o != 0 ? $this->__string($o + $this->bb_pos) : ";
    code += GenDefaultValue(field) + ";\n";
    code += Indent + "}\n\n";
  }

  // Get the value of a union from an object.
  void GetUnionField(const FieldDef &field, std::string *code_ptr) {
    std::string &code = *code_ptr;

    code += Indent + "/**\n";
    code += Indent + " * @return " + GenTypeBasic(field.value.type) + "\n";
    code += Indent + " */\n";
    code +=
        Indent + "public function " + namer_.Method("get", field) + "($obj)\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$o = $this->__offset(" +
            NumToString(field.value.offset) + ");\n";
    code += Indent + Indent;
    code += "return $o != 0 ? $this->__union($obj, $o) : null;\n";
    code += Indent + "}\n\n";
  }

  // Get the value of a vector's struct member.
  void GetMemberOfVectorOfStruct(const StructDef &struct_def,
                                 const FieldDef &field, std::string *code_ptr) {
    std::string &code = *code_ptr;
    auto vectortype = field.value.type.VectorType();

    code += Indent + "/**\n";
    code += Indent + " * @return " + GenTypeBasic(field.value.type) + "\n";
    code += Indent + " */\n";
    code +=
        Indent + "public function " + namer_.Method("get", field) + "($j)\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$o = $this->__offset(" +
            NumToString(field.value.offset) + ");\n";
    code += Indent + Indent + "$obj = new " + ScalarType(field) + "();\n";

    switch (field.value.type.base_type) {
      case BASE_TYPE_STRUCT:
        if (struct_def.fixed) {
          code += Indent + Indent;
          code += "return $o != 0 ? $obj->init($this->bb_pos +" +
                  NumToString(field.value.offset) + ", $this->bb) : null;\n";
        } else {
          code += Indent + Indent + "return $o != 0 ? $obj->init(";
          code += field.value.type.struct_def->fixed
                      ? "$o + $this->bb_pos"
                      : "$this->__indirect($o + $this->bb_pos)";
          code += ", $this->bb) : null;\n";
        }
        break;
      case BASE_TYPE_STRING:
        code += "// base_type_string\n";
        // TODO(chobie): do we need this?
        break;
      case BASE_TYPE_VECTOR:
        if (vectortype.base_type == BASE_TYPE_STRUCT) {
          code += Indent + Indent + "return $o != 0 ? $obj->init(";
          if (vectortype.struct_def->fixed) {
            code += "$this->__vector($o) + $j *";
            code += NumToString(InlineSize(vectortype));
          } else {
            code += "$this->__indirect($this->__vector($o) + $j * ";
            code += NumToString(InlineSize(vectortype)) + ")";
          }
          code += ", $this->bb) : null;\n";
        }
        break;
      case BASE_TYPE_UNION:
        code += Indent + Indent + "return $o != 0 ? $this->";
        code += GenGetter(field.value.type) + "($obj, $o); null;\n";
        break;
      default: break;
    }

    code += Indent + "}\n\n";
  }

  // Get the value of a vector's non-struct member. Uses a named return
  // argument to conveniently set the zero value for the result.
  void GetMemberOfVectorOfNonStruct(const FieldDef &field,
                                    std::string *code_ptr) {
    std::string &code = *code_ptr;
    auto vectortype = field.value.type.VectorType();
    code += Indent + "/**\n";
    code += Indent + " * @param int offset\n";
    code += Indent + " * @return " + GenTypeGet(field.value.type) + "\n";
    code += Indent + " */\n";
    code +=
        Indent + "public function " + namer_.Method("get", field) + "($j)\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$o = $this->__offset(" +
            NumToString(field.value.offset) + ");\n";

    if (IsString(field.value.type.VectorType())) {
      code += Indent + Indent;
      code += "return $o != 0 ? $this->__string($this->__vector($o) + $j * ";
      code += NumToString(InlineSize(vectortype)) + ") : ";
      code += GenDefaultValue(field) + ";\n";
    } else {
      code += Indent + Indent + "return $o != 0 ? $this->bb->";
      code += namer_.Method("get", GenTypeGet(field.value.type));
      code += "($this->__vector($o) + $j * ";
      code += NumToString(InlineSize(vectortype)) + ") : ";
      code += GenDefaultValue(field) + ";\n";
    }
    code += Indent + "}\n\n";
  }

  // Get the value of a vector's union member. Uses a named return
  // argument to conveniently set the zero value for the result.
  void GetMemberOfVectorOfUnion(const FieldDef &field, std::string *code_ptr) {
    std::string &code = *code_ptr;
    auto vectortype = field.value.type.VectorType();

    code += Indent + "/**\n";
    code += Indent + " * @param int offset\n";
    code += Indent + " * @return " + GenTypeGet(field.value.type) + "\n";
    code += Indent + " */\n";
    code += Indent + "public function " + namer_.Method("get", field) +
            "($j, $obj)\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$o = $this->__offset(" +
            NumToString(field.value.offset) + ");\n";
    code += Indent + Indent + "return $o != 0 ? ";
    code += "$this->__union($obj, $this->__vector($o) + $j * ";
    code += NumToString(InlineSize(vectortype)) + " - $this->bb_pos) : null;\n";
    code += Indent + "}\n\n";
  }

  // Recursively generate arguments for a constructor, to deal with nested
  // structs.
  void StructBuilderArgs(const StructDef &struct_def, const char *nameprefix,
                         const char *separator, std::string *code_ptr) {
    for (auto it = struct_def.fields.vec.begin();
         it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (IsStruct(field.value.type)) {
        // Generate arguments for a struct inside a struct. To ensure names
        // don't clash, and to make it obvious
        // these arguments are constructing
        // a nested struct, prefix the name with the field name.
        StructBuilderArgs(*field.value.type.struct_def,
                          (nameprefix + (field.name + separator)).c_str(),
                          separator, code_ptr);
      } else {
        std::string &code = *code_ptr;
        code += std::string(", $") + nameprefix;
        code += namer_.Variable(field);
      }
    }
  }

  // Recursively generate struct construction statements and instert manual
  // padding.
  void StructBuilderBody(const StructDef &struct_def, const char *nameprefix,
                         std::string *code_ptr) {
    std::string &code = *code_ptr;

    code += Indent + Indent + "$builder->prep(";
    code += NumToString(struct_def.minalign) + ", ";
    code += NumToString(struct_def.bytesize) + ");\n";
    for (auto it = struct_def.fields.vec.rbegin();
         it != struct_def.fields.vec.rend(); ++it) {
      auto &field = **it;
      if (field.padding) {
        code += Indent + Indent + "$builder->pad(";
        code += NumToString(field.padding) + ");\n";
      }
      if (IsStruct(field.value.type)) {
        StructBuilderBody(*field.value.type.struct_def,
                          (nameprefix + (field.name + "_")).c_str(), code_ptr);
      } else {
        code += Indent + Indent + "$builder->put" + GenMethod(field) + "($";
        code += nameprefix + namer_.Variable(field) + ");\n";
      }
    }
  }

  // Get the value of a table's starting offset.
  void GetStartOfTable(const StructDef &struct_def, std::string *code_ptr) {
    std::string &code = *code_ptr;

    code += Indent + "/**\n";
    code += Indent + " * @param FlatBufferBuilder $builder\n";
    code += Indent + " * @return void\n";
    code += Indent + " */\n";
    code += Indent + "public static function " +
            namer_.LegacyPhpMethod("start", struct_def) +
            "(FlatBufferBuilder $builder)\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$builder->startObject(";
    code += NumToString(struct_def.fields.vec.size());
    code += ");\n";
    code += Indent + "}\n\n";

    code += Indent + "/**\n";
    code += Indent + " * @param FlatBufferBuilder $builder\n";
    code += Indent + " * @return " + namer_.Type(struct_def) + "\n";
    code += Indent + " */\n";
    code += Indent + "public static function " +
            namer_.LegacyPhpMethod("create", struct_def);
    code += "(FlatBufferBuilder $builder, ";

    for (auto it = struct_def.fields.vec.begin();
         it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;

      if (field.deprecated) continue;
      if (it != struct_def.fields.vec.begin()) { code += ", "; }
      code += "$" + field.name;
    }
    code += ")\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$builder->startObject(";
    code += NumToString(struct_def.fields.vec.size());
    code += ");\n";
    for (auto it = struct_def.fields.vec.begin();
         it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (field.deprecated) continue;

      code += Indent + Indent + "self::" + namer_.Method("add", field);
      code += "($builder, $" + field.name + ");\n";
    }

    code += Indent + Indent + "$o = $builder->endObject();\n";

    for (auto it = struct_def.fields.vec.begin();
         it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (!field.deprecated && field.IsRequired()) {
        code += Indent + Indent + "$builder->required($o, ";
        code += NumToString(field.value.offset);
        code += ");  // " + field.name + "\n";
      }
    }
    code += Indent + Indent + "return $o;\n";
    code += Indent + "}\n\n";
  }

  // Set the value of a table's field.
  void BuildFieldOfTable(const FieldDef &field, const size_t offset,
                         std::string *code_ptr) {
    std::string &code = *code_ptr;

    code += Indent + "/**\n";
    code += Indent + " * @param FlatBufferBuilder $builder\n";
    code += Indent + " * @param " + GenTypeBasic(field.value.type) + "\n";
    code += Indent + " * @return void\n";
    code += Indent + " */\n";
    code += Indent + "public static function " + namer_.Method("add", field) +
            "(FlatBufferBuilder $builder, $" + namer_.Variable(field) + ")\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$builder->add";
    code += GenMethod(field) + "X(";
    code += NumToString(offset) + ", ";

    code += "$" + namer_.Variable(field);
    code += ", ";
    code += GenDefaultValue(field);

    code += ");\n";
    code += Indent + "}\n\n";
  }

  // Set the value of one of the members of a table's vector.
  void BuildVectorOfTable(const FieldDef &field, std::string *code_ptr) {
    std::string &code = *code_ptr;
    auto vector_type = field.value.type.VectorType();
    auto alignment = InlineAlignment(vector_type);
    auto elem_size = InlineSize(vector_type);

    code += Indent + "/**\n";
    code += Indent + " * @param FlatBufferBuilder $builder\n";
    code += Indent + " * @param array offset array\n";
    code += Indent + " * @return int vector offset\n";
    code += Indent + " */\n";
    code += Indent + "public static function ";
    code += namer_.Method("create", field, "Vector");
    code += "(FlatBufferBuilder $builder, array $data)\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$builder->startVector(";
    code += NumToString(elem_size);
    code += ", count($data), " + NumToString(alignment);
    code += ");\n";
    code += Indent + Indent;
    code += "for ($i = count($data) - 1; $i >= 0; $i--) {\n";
    if (IsScalar(field.value.type.VectorType().base_type)) {
      code += Indent + Indent + Indent;
      code += "$builder->";
      code += namer_.Method("put", GenTypeBasic(field.value.type.VectorType()));
      code += "($data[$i]);\n";
    } else {
      code += Indent + Indent + Indent;
      code += "$builder->putOffset($data[$i]);\n";
    }
    code += Indent + Indent + "}\n";
    code += Indent + Indent + "return $builder->endVector();\n";
    code += Indent + "}\n\n";

    code += Indent + "/**\n";
    code += Indent + " * @param FlatBufferBuilder $builder\n";
    code += Indent + " * @param int $numElems\n";
    code += Indent + " * @return void\n";
    code += Indent + " */\n";
    code += Indent + "public static function ";
    code += namer_.Method("start", field, "Vector");
    code += "(FlatBufferBuilder $builder, $numElems)\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$builder->startVector(";
    code += NumToString(elem_size);
    code += ", $numElems, " + NumToString(alignment);
    code += ");\n";
    code += Indent + "}\n\n";
  }

  // Get the offset of the end of a table.
  void GetEndOffsetOnTable(const StructDef &struct_def, std::string *code_ptr) {
    std::string &code = *code_ptr;

    code += Indent + "/**\n";
    code += Indent + " * @param FlatBufferBuilder $builder\n";
    code += Indent + " * @return int table offset\n";
    code += Indent + " */\n";
    code += Indent + "public static function " +
            namer_.LegacyPhpMethod("end", struct_def);
    code += "(FlatBufferBuilder $builder)\n";
    code += Indent + "{\n";
    code += Indent + Indent + "$o = $builder->endObject();\n";

    for (auto it = struct_def.fields.vec.begin();
         it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (!field.deprecated && field.IsRequired()) {
        code += Indent + Indent + "$builder->required($o, ";
        code += NumToString(field.value.offset);
        code += ");  // " + field.name + "\n";
      }
    }
    code += Indent + Indent + "return $o;\n";
    code += Indent + "}\n";

    GenerateFinisher(struct_def, code, false);
    GenerateFinisher(struct_def, code, true);
  }

  void GenerateFinisher(const StructDef &struct_def, std::string &code,
                        bool size_prefixed) {
    if (parser_.root_struct_def_ == &struct_def) {
      std::string sizePrefixed(size_prefixed ? "SizePrefixed" : "");

      code += "\n";
      code += Indent + "public static function ";
      code += "finish" + sizePrefixed + struct_def.name;
      code += "Buffer(FlatBufferBuilder $builder, $offset)\n";
      code += Indent + "{\n";
      code += Indent + Indent + "$builder->finish($offset";

      if (!parser_.file_identifier_.empty()) {
        code += ", \"" + parser_.file_identifier_ + "\"";
      }
      if (size_prefixed) {
        if (parser_.file_identifier_.empty()) { code += ", null"; }
        code += ", true";
      }
      code += ");\n";
      code += Indent + "}\n";
    }
  }

  // Generate a struct field, conditioned on its child type(s).
  void GenStructAccessor(const StructDef &struct_def, const FieldDef &field,
                         std::string *code_ptr) {
    GenComment(field.doc_comment, code_ptr, nullptr, Indent.c_str());

    if (IsScalar(field.value.type.base_type)) {
      if (struct_def.fixed) {
        GetScalarFieldOfStruct(field, code_ptr);
      } else {
        GetScalarFieldOfTable(field, code_ptr);
      }
    } else {
      switch (field.value.type.base_type) {
        case BASE_TYPE_STRUCT:
          if (struct_def.fixed) {
            GetStructFieldOfStruct(field, code_ptr);
          } else {
            GetStructFieldOfTable(field, code_ptr);
          }
          break;
        case BASE_TYPE_STRING: GetStringField(field, code_ptr); break;
        case BASE_TYPE_VECTOR: {
          auto vectortype = field.value.type.VectorType();
          if (vectortype.base_type == BASE_TYPE_UNION) {
            GetMemberOfVectorOfUnion(field, code_ptr);
          } else if (vectortype.base_type == BASE_TYPE_STRUCT) {
            GetMemberOfVectorOfStruct(struct_def, field, code_ptr);
          } else {
            GetMemberOfVectorOfNonStruct(field, code_ptr);
          }
          break;
        }
        case BASE_TYPE_UNION: GetUnionField(field, code_ptr); break;
        default: FLATBUFFERS_ASSERT(0);
      }
    }
    if (IsVector(field.value.type)) {
      GetVectorLen(field, code_ptr);
      if (field.value.type.element == BASE_TYPE_UCHAR) {
        GetUByte(field, code_ptr);
      }
    }
  }

  // Generate table constructors, conditioned on its members' types.
  void GenTableBuilders(const StructDef &struct_def, std::string *code_ptr) {
    GetStartOfTable(struct_def, code_ptr);

    for (auto it = struct_def.fields.vec.begin();
         it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (field.deprecated) continue;

      auto offset = it - struct_def.fields.vec.begin();
      if (field.value.type.base_type == BASE_TYPE_UNION) {
        std::string &code = *code_ptr;
        code += Indent + "public static function ";
        code += namer_.Method("add", field);
        code += "(FlatBufferBuilder $builder, $offset)\n";
        code += Indent + "{\n";
        code += Indent + Indent + "$builder->addOffsetX(";
        code += NumToString(offset) + ", $offset, 0);\n";
        code += Indent + "}\n\n";
      } else {
        BuildFieldOfTable(field, offset, code_ptr);
      }
      if (IsVector(field.value.type)) { BuildVectorOfTable(field, code_ptr); }
    }

    GetEndOffsetOnTable(struct_def, code_ptr);
  }

  // Generate struct or table methods.
  void GenStruct(const StructDef &struct_def, std::string *code_ptr) {
    if (struct_def.generated) return;

    if (parser_.opts.generate_object_based_api) {
      GenNativeStruct(struct_def, code_ptr);
    }
    GenComment(struct_def.doc_comment, code_ptr, nullptr);
    BeginClass(struct_def, code_ptr);

    if (!struct_def.fixed) {
      // Generate a special accessor for the table that has been declared as
      // the root type.
      NewRootTypeFromBuffer(struct_def, code_ptr, false);
      NewRootTypeFromBuffer(struct_def, code_ptr, true);
    }

    std::string &code = *code_ptr;
    if (!struct_def.fixed) {
      if (parser_.file_identifier_.length()) {
        // Return the identifier
        code += Indent + "public static function " +
                namer_.LegacyPhpMethod(struct_def, "Identifier") + "()\n";
        code += Indent + "{\n";
        code += Indent + Indent + "return \"";
        code += parser_.file_identifier_ + "\";\n";
        code += Indent + "}\n\n";

        // Check if a buffer has the identifier.
        code += Indent + "public static function " +
                namer_.LegacyPhpMethod(struct_def, "BufferHasIdentifier") +
                "(ByteBuffer $buf)\n";
        code += Indent + "{\n";
        code += Indent + Indent + "return self::";
        code += "__has_identifier($buf, self::";
        code += namer_.LegacyPhpMethod(struct_def, "Identifier") + "());\n";
        code += Indent + "}\n\n";
      }

      if (parser_.file_extension_.length()) {
        // Return the extension
        code += Indent + "public static function " +
                namer_.LegacyPhpMethod(struct_def, "Extension") + "()\n";
        code += Indent + "{\n";
        code += Indent + Indent + "return \"" + parser_.file_extension_;
        code += "\";\n";
        code += Indent + "}\n\n";
      }
    }

    // Generate the Init method that sets the field in a pre-existing
    // accessor object. This is to allow object reuse.
    InitializeExisting(struct_def, code_ptr);
    for (auto it = struct_def.fields.vec.begin();
         it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (field.deprecated) continue;

      GenStructAccessor(struct_def, field, code_ptr);
    }

    if (struct_def.fixed) {
      // create a struct constructor function
      GenStructBuilder(struct_def, code_ptr);
    } else {
      // Create a set of functions that allow table construction.
      GenTableBuilders(struct_def, code_ptr);
    }
    if (parser_.opts.generate_object_based_api) {
      GenNativeUnpack(struct_def, code_ptr);
    }
    EndClass(code_ptr);
  }

  // Generate enum declarations.
  void GenEnum(const EnumDef &enum_def, std::string *code_ptr) {
    if (enum_def.generated) return;

    std::string &code = *code_ptr;

    if (enum_def.is_union && parser_.opts.generate_object_based_api) {
      code += "use \\Google\\FlatBuffers\\FlatBufferBuilder;\n";
      code += "\n";
      GenNativeUnion(enum_def, code_ptr);
    }

    GenComment(enum_def.doc_comment, code_ptr, nullptr);
    BeginEnum(enum_def.name, code_ptr);
    for (auto it = enum_def.Vals().begin(); it != enum_def.Vals().end(); ++it) {
      auto &ev = **it;
      GenComment(ev.doc_comment, code_ptr, nullptr, Indent.c_str());
      EnumMember(enum_def, ev, code_ptr);
    }

    code += "\n";
    code += Indent + "private static $names = array(\n";
    for (auto it = enum_def.Vals().begin(); it != enum_def.Vals().end(); ++it) {
      auto &ev = **it;
      code += Indent + Indent + enum_def.name + "::" + ev.name + "=>" + "\"" +
              ev.name + "\",\n";
    }

    code += Indent + ");\n\n";
    code += Indent + "public static function Name($e)\n";
    code += Indent + "{\n";
    code += Indent + Indent + "if (!isset(self::$names[$e])) {\n";
    code += Indent + Indent + Indent + "throw new \\Exception();\n";
    code += Indent + Indent + "}\n";
    code += Indent + Indent + "return self::$names[$e];\n";
    code += Indent + "}\n";

    if (enum_def.is_union && parser_.opts.generate_object_based_api) {
      GenNativeUnionUnPack(enum_def, code_ptr);
    }
    EndEnum(code_ptr);
  }

  // Returns the function name that is able to read a value of the given type.
  std::string GenGetter(const Type &type) const {
    switch (type.base_type) {
      case BASE_TYPE_STRING: return "__string";
      case BASE_TYPE_STRUCT: return "__struct";
      case BASE_TYPE_UNION: return "__union";
      case BASE_TYPE_VECTOR: return GenGetter(type.VectorType());
      default: return "Get";
    }
  }

  // Returns the method name for use with add/put calls.
  std::string GenMethod(const FieldDef &field) {
    return IsScalar(field.value.type.base_type)
               ? ConvertCase(GenTypeBasic(field.value.type), Case::kUpperCamel)
               : (IsStruct(field.value.type) ? "Struct" : "Offset");
  }

  std::string GenTypeBasic(const Type &type) const {
    // clang-format off
    static const char *ctypename[] = {
      #define FLATBUFFERS_TD(ENUM, IDLTYPE, \
              CTYPE, JTYPE, GTYPE, NTYPE, ...) \
        #NTYPE,
        FLATBUFFERS_GEN_TYPES(FLATBUFFERS_TD)
      #undef FLATBUFFERS_TD
    };
    // clang-format on
    return ctypename[type.base_type];
  }

  std::string GenDefaultValue(const FieldDef &field) {
    if (field.IsScalarOptional()) { return "null"; }

    const auto &value = field.value;

    if (value.type.enum_def) {
      if (auto val = value.type.enum_def->FindByValue(value.constant)) {
        return WrapInNameSpace(*value.type.enum_def) + "::" + val->name;
      }
    }
    if (IsVector(value.type) || IsArray(value.type)) {
      return GenDefaultValueBasic(field);
    }
    return GenDefaultValueBasic(field);
  }

  std::string GenDefaultValueBasic(const FieldDef &field) {
    switch (field.value.type.base_type) {
      case BASE_TYPE_BOOL:
        return field.value.constant == "0" ? "false" : "true";

      case BASE_TYPE_VECTOR:
      case BASE_TYPE_ARRAY: return "array()";

      case BASE_TYPE_UNION:
      case BASE_TYPE_STRUCT:
      case BASE_TYPE_STRING: return "null";

      case BASE_TYPE_FLOAT:
      case BASE_TYPE_DOUBLE: {
        return float_const_gen_.GenFloatConstant(field);
      }

      case BASE_TYPE_LONG:
      case BASE_TYPE_ULONG:
        if (field.value.constant != "0") {
          int64_t constant = StringToInt(field.value.constant.c_str());
          return NumToString(constant);
        }
        return "0";

      default: return field.value.constant;
    }
  }

  std::string GenTypePointer(const Type &type) {
    switch (type.base_type) {
      case BASE_TYPE_STRING: return "string";
      case BASE_TYPE_VECTOR: return GenTypeGet(type.VectorType());
      case BASE_TYPE_STRUCT: return namer_.Type(*type.struct_def);
      case BASE_TYPE_UNION:
        // fall through
      default: return "Table";
    }
  }

  std::string GenTypeGet(const Type &type) {
    return IsScalar(type.base_type) ? GenTypeBasic(type) : GenTypePointer(type);
  }

  std::string GetMethod(const StructDef &struct_def,
                        const FieldDef &field) const {
    return field.IsScalar() && struct_def.fixed
               ? namer_.LegacyPhpField(GenGetter(field.value.type), field)
               : namer_.Method("get", field);
  }

  // Create a struct with a builder and the struct's arguments.
  void GenStructBuilder(const StructDef &struct_def, std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += "\n";
    code += Indent + "/**\n";
    code += Indent + " * @return int offset\n";
    code += Indent + " */\n";
    code += Indent + "public static function " +
            namer_.LegacyPhpMethod("create", struct_def);
    code += "(FlatBufferBuilder $builder";
    StructBuilderArgs(struct_def, "", "_", code_ptr);
    code += ")\n";
    code += Indent + "{\n";

    StructBuilderBody(struct_def, "", code_ptr);

    code += Indent + Indent + "return $builder->offset();\n";
    code += Indent + "}\n";
  }

  std::string ScalarType(const FieldDef &field) {
    return ConvertCase(GenTypeGet(field.value.type), Case::kUpperCamel);
  }

  SimpleFloatConstantGenerator float_const_gen_;
  IdlNamer namer_;
};
}  // namespace php

static bool GeneratePhp(const Parser &parser, const std::string &path,
                        const std::string &file_name) {
  php::PhpGenerator generator(parser, path, file_name);
  return generator.generate();
}

namespace {

class PhpCodeGenerator : public CodeGenerator {
 public:
  Status GenerateCode(const Parser &parser, const std::string &path,
                      const std::string &filename) override {
    if (!GeneratePhp(parser, path, filename)) { return Status::ERROR; }
    return Status::OK;
  }

  Status GenerateCode(const uint8_t *, int64_t,
                      const CodeGenOptions &) override {
    return Status::NOT_IMPLEMENTED;
  }

  Status GenerateMakeRule(const Parser &parser, const std::string &path,
                          const std::string &filename,
                          std::string &output) override {
    (void)parser;
    (void)path;
    (void)filename;
    (void)output;
    return Status::NOT_IMPLEMENTED;
  }

  Status GenerateGrpcCode(const Parser &parser, const std::string &path,
                          const std::string &filename) override {
    (void)parser;
    (void)path;
    (void)filename;
    return Status::NOT_IMPLEMENTED;
  }

  Status GenerateRootFile(const Parser &parser,
                          const std::string &path) override {
    (void)parser;
    (void)path;
    return Status::NOT_IMPLEMENTED;
  }

  bool IsSchemaOnly() const override { return true; }

  bool SupportsBfbsGeneration() const override { return false; }

  bool SupportsRootFileGeneration() const override { return false; }

  IDLOptions::Language Language() const override { return IDLOptions::kPhp; }

  std::string LanguageName() const override { return "Php"; }
};
}  // namespace

std::unique_ptr<CodeGenerator> NewPhpCodeGenerator() {
  return std::unique_ptr<PhpCodeGenerator>(new PhpCodeGenerator());
}

}  // namespace flatbuffers
