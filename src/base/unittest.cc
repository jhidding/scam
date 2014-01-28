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

#include "unittest.hh"
#include "longstring.hh"

using namespace Test;

std::unique_ptr<std::map<std::string, Test::Unit const *>> Test::Unit::_instances;

Unit::imap &Unit::instances()
{
	if (not _instances)
		_instances = std::unique_ptr<imap>(new imap);

	return *_instances;
}

void Unit::all(bool should_break)
{
	for (auto &kv : instances())
	{
		std::cerr << "[test \033[34;1m" << kv.first << "\033[m]\n";
		if (not run_test(kv.second) and should_break)
			break;
	}
}

bool Test::run_test(Unit const *unit)
{
	bool pass;
	try
	{
		pass = (*unit)();
	}
	catch (char const *e)
	{
		pass = false;
		std::cerr << "failure: " << e << std::endl;
	}
	catch (std::string const &e)
	{
		pass = false;
		std::cerr << "failure: " << e << std::endl;
	}

	if (pass)
	{
		std::cerr << "\033[62G[\033[32mpassed\033[m]\n";
	}
	else
	{
		std::cerr << "\033[62G[\033[31mfailed\033[m]\n"
			<< Misc::LongString(unit->description(), 72,
					[] () -> std::string { return " | "; })
			<< std::endl;
	}

	return pass;
}

Unit::Unit(	
	std::string const &name_, 
	std::string const &description_,
	std::function<bool ()> const &code_ ):

	_name(name_), 
	_description(description_),
	code(code_)
{
	instances()[_name] = this;
}

Unit::~Unit()
{
	instances().erase(_name);
}

// vim:sw=4:ts=4:tw=72
