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

#include "argv.hh"

using namespace System;

std::string System::Argv::select(std::string const &name, unsigned N) const
{
	if (N >= size())
	{
		std::ostringstream ss;
		ss << "Trying to find option [" << name << "], but no such option exists.";
		throw ss.str();
	}

	if ((*this)[N].long_name == name)
		return (*this)[N].value;
	else
		return select(name, N+1);
}

bool System::Argv::pass() const
{
	size_t n = std::count_if(begin(), end(), 
		[] (Option const &o) { return o.checked() or not o.valued(); });
	return (n == size());
}

int System::Argv::parse(int argc, char **argv, int n)
{
	_command_line = argv[0];
	for (int i = 1; i < argc; ++i)
		_command_line += std::string(" ") + argv[i];

	while (n != argc)
	{
		std::string arg(argv[n]);
		iterator option;

		if (arg[0] != '-')
		{
			return n;
		}

		if (arg[1] != '-')
		{
			std::string name = arg.substr(1, 100);
			option = std::find_if(begin(), end(),
				[name] (Option &o) { return o.short_name == name; });
		}
		else
		{
			std::string name = arg.substr(2, 100);
			option = std::find_if(begin(), end(),
				[name] (Option &o) { return o.long_name == name; });
		}

		if (option == end())
		{
			std::ostringstream ss;
			ss << "Got the argument [" << arg << "], but no such option exists.";
			throw ss.str();
		}

		if (option->valued() and (argc - n) < 2)
		{
			std::ostringstream ss;
			ss << "Got the last argument [" << arg << "], but no value given.";
			throw ss.str();
		}

		if (option->valued())
		{
			option->value = argv[n+1];
			option->check();
			n += 2;
		}
		else
		{
			option->check();
			n += 1;
		}
	}

	return n;
}

void System::Argv::print(std::ostream &out) const
{
	for (auto &o : *this)
	{
		out << o << std::endl;
	}
}

std::ostream &System::operator<<(std::ostream &out, Option const &o)
{
	std::ostringstream ss("        ", std::ios::ate);
	ss << "--" << o.long_name;
	if (o.short_name.length() > 0)
		ss << " | -" << o.short_name;
	if (o.valued() and o.checked())
		ss << " (default: " << o.value << ")";
	if (o.valued() and not o.checked())
		ss << " (obligatory)";
	if (not o.valued())
		ss << " (flag)";
	ss << " : ";

	std::vector<std::string> words;
	split(o.description, ' ', std::back_inserter(words));

	auto i = words.begin();
	while (i != words.end())
	{
		do {
			ss << *i << " ";
			++i;
		} while (ss.str().length() < 72 and i != words.end());

		out << ss.str() << std::endl;
		ss.str("                ");
	}

	return out;
}

// vim:sw=4:ts=4:tw=72
