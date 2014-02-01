#include "base.hh"

template <> std::string PLY::Type<int8_t  >::name = "char";
template <> std::string PLY::Type<uint8_t >::name = "uchar";
template <> std::string PLY::Type<int16_t >::name = "short";
template <> std::string PLY::Type<uint16_t>::name = "ushort";	
template <> std::string PLY::Type<int32_t >::name = "int";
template <> std::string PLY::Type<uint32_t>::name = "uint";
template <> std::string PLY::Type<float   >::name = "float";
template <> std::string PLY::Type<double  >::name = "double";

template <> PLY::TYPE_ID PLY::Type<int8_t  >::id = PLY::T_CHAR;
template <> PLY::TYPE_ID PLY::Type<uint8_t >::id = PLY::T_UCHAR;
template <> PLY::TYPE_ID PLY::Type<int16_t >::id = PLY::T_SHORT;
template <> PLY::TYPE_ID PLY::Type<uint16_t>::id = PLY::T_USHORT;
template <> PLY::TYPE_ID PLY::Type<int32_t >::id = PLY::T_INT;
template <> PLY::TYPE_ID PLY::Type<uint32_t>::id = PLY::T_UINT;
template <> PLY::TYPE_ID PLY::Type<float   >::id = PLY::T_FLOAT;
template <> PLY::TYPE_ID PLY::Type<double  >::id = PLY::T_DOUBLE;

std::map<PLY::TYPE_ID, PLY::Type_info> PLY::Type_map = {
	{ PLY::T_CHAR,		{ "char",   1 } },
        { PLY::T_UCHAR,		{ "uchar",  1 } },
        { PLY::T_SHORT,		{ "short",  2 } },
        { PLY::T_USHORT,	{ "ushort", 2 } },
        { PLY::T_INT,           { "int",    4 } },
        { PLY::T_UINT,		{ "uint",   4 } },
        { PLY::T_FLOAT,		{ "float",  4 } },
        { PLY::T_DOUBLE,	{ "double", 8 } } };

