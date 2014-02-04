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
#include <gtkmm/button.h>
#include <gtkmm/window.h>
#include <gtksourceviewmm/view.h>
#include <gtksourceviewmm/buffer.h>

namespace GUI
{
	class HelloWorld: public Gtk::Window
	{
		public:
			HelloWorld();
			virtual ~HelloWorld();

		protected:
			Gsv::View m_view;
	};

	HelloWorld::HelloWorld():
		m_view(Gsv::Buffer::create())
	{
		add(m_view);
		m_view.show();
	}

	HelloWorld::~HelloWorld() {}

	void command_gui(int argc, char **argv)
	{	
		Gtk::Main kit(argc, argv);

		HelloWorld helloworld;
		Gtk::Main::run(helloworld);
	}
}

#include "../base/global.hh"
System::Global<Command> _COMMAND_GUI("gui", GUI::command_gui);

