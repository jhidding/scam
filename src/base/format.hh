/*  This file is part of Scarlett.

    Scarlett is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Scarlett is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Scarlett.  If not, see <http://www.gnu.org/licenses/>. */

#pragma once

#include <string>
#include <sstream>
#include <utility>

namespace Misc
{
	inline void format_to(std::ostream &out) {}

	template <typename First, typename ...Rest>
	void format_to(std::ostream &out, First a, Rest &&...rest)
	{
		out << a;
		format_to(out, std::forward<Rest>(rest)...);
	}

	template <typename ...Args>
	std::string format(Args &&...args)
	{
		std::ostringstream ss;
		format_to(ss, std::forward<Args>(args)...);
		return ss.str();
	}
}

