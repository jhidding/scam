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

#include <vector>
#include <string>
#include <utility>

#include <iostream>
#include <sstream>
#include <algorithm>

#include <cstdlib>

/* Extensive command-line configuration framework */

namespace System
{
	struct Option
	{
		enum Flags { CHECK = 1, VALUED = 2 };

		int flags;
		std::string short_name, long_name, value, description;

		bool checked() const { return (flags & CHECK) > 0; }
		bool valued() const { return (flags & VALUED) > 0; }
		void check() 
		{ 
			flags |= CHECK;
			if (not valued())
				value = "1";
		}

		Option(int f, std::string const &sn, std::string const &ln,
				std::string const &dv, std::string const &de):
			flags(f), short_name(sn), long_name(ln),
			value(dv), description(de) {}
	};

	template <typename I>
	void split(std::string const &s, char d, I inserter)
	{
		size_t p, q = 0;
		while (q < s.length())
		{
			p = s.find_first_not_of(d, q);
			if (p == std::string::npos) return;

			q = s.find_first_of(d, p);
			if (q == std::string::npos)
			{
				*inserter = s.substr(p, s.length() - p);
				return;
			}
			else
			{
				*inserter = s.substr(p, q-p);
				++inserter;
			}
		}
	}

	extern std::ostream &operator<<(std::ostream &out, Option const &o);

	class Argv: public std::vector<Option>
	{
		std::string _command_line;

		public:
			template <typename ...Args>
			Argv(Args&&... opts):
				std::vector<Option>({opts...})
			{}

			template <typename T>
			T get(std::string const &name) const
			{
				std::istringstream ss(select(name, 0));
				T value;
				ss >> value;
				return value;
			}

			std::string operator[](std::string const &name) const
			{ 
				return select(name, 0); 
			}

			Option const &operator[](unsigned i) const
			{
				return std::vector<Option>::operator[](i);
			}

			std::string const &command_line() const
			{
				return _command_line;
			}

			std::string select(std::string const &name, unsigned N) const;
			bool pass() const;
			int parse(int argc, char **argv, int n = 1);
			void print(std::ostream &out) const;
	};

	template <typename ...Args>
	Argv read_arguments(int argc, char **argv, Args &&...args)
	{
		Argv C(std::forward<Args>(args)...);

		try
		{
			int n = C.parse(argc, argv);

			if (n != argc)
			{
				std::ostringstream ss;
				ss << "Don't know how to handle [" << argv[n] << "]";
				throw ss.str();
			}
		}
		catch (std::string const &error)
		{
			std::cout << "ERROR: " << error << "\n";
			exit(-1);
		}

		return C;
	}
}

// vim:sw=4:ts=4:tw=72
