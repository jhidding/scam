#pragma once
#include <string>
#include <memory>
#include <stdint.h>
#include <stdexcept>
#include <map>
#include <vector>

namespace PLY
{
	class Exception: public std::exception
	{
		std::string msg;

		public:
			Exception(std::string const &msg_): msg(msg_) {}
			char const *what() const throw () { return msg.c_str(); }
			~Exception() throw () {}
	};

	enum Format { ASCII, BINARY };

	template <typename T> using ptr = std::unique_ptr<T>;

	enum TYPE_ID { T_CHAR, T_UCHAR, T_SHORT, T_USHORT, 
		       T_INT,  T_UINT,  T_FLOAT, T_DOUBLE  };

	template <typename T>
	struct Type {
		static TYPE_ID     id;
		static std::string name;
	};

	struct Type_info
	{
		std::string name;
		unsigned    size;
	};

	extern std::map<TYPE_ID, Type_info> Type_map;

	class ByteVector
	{
		std::vector<char>	bytes;
		TYPE_ID 		type_id;
		unsigned 		type_size;

		template <typename T, typename U>
		static T cast(char const *d)
		{
			return static_cast<T>(*reinterpret_cast<U const *>(d));
		}

		template <typename T>
		T caster(char const *d) const
		{
			switch (type_id)
			{
				case T_CHAR:	return cast<T, int8_t  >(d);
				case T_UCHAR: 	return cast<T, uint8_t >(d);
				case T_SHORT: 	return cast<T, int16_t >(d);
				case T_USHORT: 	return cast<T, uint16_t>(d);
				case T_INT: 	return cast<T, int32_t >(d);
				case T_UINT: 	return cast<T, uint32_t>(d);
				case T_FLOAT: 	return cast<T, float   >(d);
				case T_DOUBLE:  return cast<T, double  >(d);
				default: throw Exception("Error in ByteVector typecast.");
			}
		};

		public:
			ByteVector() {}
			ByteVector(std::vector<char> bytes_, TYPE_ID type_id_):
				bytes(bytes_), type_id(type_id_), type_size(Type_map[type_id].size) 
			{}	
				
			template <typename T>
			T as() const 
			{ 
				return caster<T>(bytes.data()); 
			}

			size_t size() const 
			{ 
				return bytes.size() / type_size; 
			}

			template <typename T>
			T idx(unsigned i) const 
			{ 
				return caster<T>(bytes.data() + i*type_size);
			}

			template <typename T>
			std::vector<T> as_vector() const 
			{
				std::vector<T> result;
				for (unsigned i = 0; i < size(); ++i)
					result.push_back(idx<T>(i));
				return result;
			}
	};
}

