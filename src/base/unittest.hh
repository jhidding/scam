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

#include <set>
#include <string>
#include <functional>
#include <map>
#include <memory>

namespace Test
{
	class Unit
	{
		typedef std::map<std::string, Unit const *> imap;
		static std::unique_ptr<imap> _instances;

		std::string const _name, _description;
		std::function<bool ()> const code;

		public:
			static imap &instances();
			static void all(bool);

			~Unit();
			Unit(
				std::string const &name_, 
				std::string const &description_,
				std::function<bool ()> const &code_ );

			bool operator()() const
			{
				return code();
			}

			std::string const &description() const
			{
				return _description;
			}

			std::string const &name() const
			{
				return _name;
			}
	};

	extern bool run_test(Unit const *unit);
}

// vim:sw=4:ts=4:tw=72
