#include "base.hh"

template <> std::string PLY::Type<int8_t>::name = "char";
template <> std::string PLY::Type<uint8_t>::name = "uchar";
template <> std::string PLY::Type<int16_t>::name = "short";
template <> std::string PLY::Type<uint16_t>::name = "ushort";	
template <> std::string PLY::Type<int32_t>::name = "int";
template <> std::string PLY::Type<uint32_t>::name = "uint";
template <> std::string PLY::Type<float>::name = "float";
template <> std::string PLY::Type<double>::name = "double";

