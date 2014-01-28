/*  This file is part of Ares.

    Ares is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Ares is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Ares.  If not, see <http://www.gnu.org/licenses/>. */

#pragma once
#include <map>
#include <string>
#include <memory>
#include <utility>
#include <vector>

namespace System
{
	template <typename T>
	class Global: public T
	{
		typedef std::map<std::string, T *> tmap;
		static std::unique_ptr<tmap> _dir;

		public:
			static tmap &dir() 
			{
				if (not _dir)
					_dir = std::unique_ptr<tmap>(new tmap);

				return *_dir; 
			}

			static std::vector<std::string> items()
			{
				std::vector<std::string> V;
				for (auto &kv : dir())
					V.push_back(kv.first);

				return V;
			}

			static T &get(std::string const &name)
			{
				auto i = dir().find(name);
				if (i == dir().end())
					throw "command [" + name + "] not understood.";

				return *(i->second);
			}

			template <typename ...Args>
			Global(std::string const &name, Args &&...args):
				T(std::forward<Args>(args)...)
			{
				dir()[name] = this;
			}
	};

	template <typename T>
	std::unique_ptr<std::map<std::string, T*>> Global<T>::_dir;
}

// vim:sw=4:ts=4:tw=72
