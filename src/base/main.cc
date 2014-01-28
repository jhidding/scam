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

#include "common.hh"
#include "argv.hh"
#include "global.hh"

using namespace System;

#ifdef UNITTEST
#include "unittest.hh"
#include "longstring.hh"

namespace Test
{
	void command_test(int argc_, char **argv_)
	{
		Argv argv = read_arguments(argc_, argv_,
			Option({0, "h", "help", "false", 
				"print this help."}),
			Option({0, "l", "list", "false", 
				"list available tests."}),
			Option({0, "n", "no-break", "false",
				"don't break after the first failed test."}),
			Option({Option::VALUED | Option::CHECK, "s", "select", "-", 
				"select a test to run, if '-' is given, all tests "
				"are run in alphanumerical order."}));

		if (argv.get<bool>("help"))
		{
			std::cerr << "Scam -- 3D vector graphics for science.\n"
				"Copyright Johan Hidding, June 2014 - licence: GPL3.\n\n";
			argv.print(std::cerr);
			exit(0);
		}

		if (argv.get<bool>("list"))
		{
			std::cerr << "Scam -- 3D vector graphics for science.\n"
				"Copyright Johan Hidding, June 2014 - licence: GPL3.\n\n"
				"available tests:\n";

			for (auto &kv : Unit::instances())
			{
				std::cerr << kv.first << std::endl
					<< Misc::LongString(kv.second->description(), 72,
							[] () -> std::string { return " | "; })
					<< std::endl;
			}

			exit(0);
		}

		bool should_break = not argv.get<bool>("no-break");

		if (argv["select"] != "-")
		{
			run_test(Unit::instances()[argv["select"]]);
		}
		else
		{
			Unit::all(should_break);
		}
	}

	Global<Command> _COMMAND_TEST_("test", command_test);
}

#endif

void usage(std::string const &cmd)
{
	std::cerr << "Scam -- 3D vector graphics for science.\n"
		"Copyright Johan Hidding, June 2014 - licence: GPL3.\n\n"
		"usage: " << cmd << " [command] [args]\n"
		"where [command] is one of: " << System::join(Global<Command>::items(), ", ") <<
		"\nto get help on a command: " << cmd << " [command] --help\n";

	exit(0);
}

int main(int argc, char **argv)
{
	if (argc < 2) usage(argv[0]);

	std::string command(argv[1]);

	try {
		Global<Command>::get(command)(argc - 1, argv + 1);
		exit(0);
	}

	catch (char const *msg) {
		std::cerr << "[error] " << msg << std::endl;
		exit(-1);
	}

	catch (std::string const &msg) {
		std::cerr << "[error] " << msg << std::endl;
		exit(-1);
	}

	return 0;
}

// vim:sw=4:ts=4:tw=72
