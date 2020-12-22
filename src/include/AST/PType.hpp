#ifndef __AST_PTYPE_H
#define __AST_PTYPE_H

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

class PType;

typedef std::shared_ptr<const PType> PTypeSharedPtr;

class PType {
   public:
    enum class PrimitiveTypeEnum : uint8_t {
        kVoidType,
        kIntegerType,
        kRealType,
        kBoolType,
        kStringType,
        kUnknown
    };

    PType(PrimitiveTypeEnum type);
    ~PType() = default;

    void setDimensions(const std::vector<uint64_t> &dims);

    const PrimitiveTypeEnum getPrimitiveType() const;
    const char *getPTypeCString() const;
    const std::vector<uint64_t> getDimensions() const;
    bool operator!=(const PType &) const;
    bool operator==(const PType &) const;

   private:
    PrimitiveTypeEnum type;
    std::vector<uint64_t> dimensions;
    mutable std::string type_string;
    mutable bool type_string_is_valid = false;
};

typedef PType::PrimitiveTypeEnum Prim;

#endif
