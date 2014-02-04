#include "../base/common.hh"
#include "../base/argv.hh"
#include "../base/format.hh"
#include "../ply/ply.hh"
#include "../ply/read.hh"
#include "../geometry/geometry.hh"
#include "../render/render.hh"
#include "../render/map_projection.hh"
#include "../material/colour.hh"

#include <ctime>
#include <sstream>
#include <iomanip>

using namespace System;
using namespace Scam;

#include <gtkmm.h>

namespace GUI
{
	void command_gui(int argc, char **argv)
	{	
		Gtk::Main kit(argc, argv);

		Gtk::Window window;

		Gtk::Main::run(window);
	}
}

#include "../base/global.hh"
System::Global<Command> _COMMAND_GUI("gui", GUI::command_gui);

