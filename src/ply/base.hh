#pragma once
#include <string>
#include <memory>
#include <stdint.h>

namespace PLY
{
	enum Format { ASCII, BINARY };

	template <typename T> using ptr = std::unique_ptr<T>;

	template <typename T>
	struct Type { 
		static std::string name; 
	};
}

