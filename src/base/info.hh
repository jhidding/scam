#pragma once
#include <map>
#include <string>
#include <sstream>

#include "common.hh"

namespace Scam
{
	class Info
	{
		using Info_t = std::map<std::string, std::string>;
		Scam::ptr<Info_t> m_info;

		public:
			template <typename T>
			void set(std::string const &key, T const &value)
			{
				if (not m_info) m_info = make_ptr<Info_t>();
				std::ostringstream ss; ss << value;
				(*m_info)[key] = ss.str();
			}

			template <typename T>
			Maybe<T> get(std::string const &key) const
			{
				if (not m_info) return Nothing;
				auto i = m_info->find(key);
				if (i == m_info->end()) return Nothing;
				std::istringstream ss(i->second);
				T value; ss >> value;
				return value;
			}
	};
}

