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

			void on_menu_file_quit() { hide(); }
			void on_menu_file_new_generic() { std::cout << "File|New " << std::endl; }
			void on_menu_others() { std::cout << "other" << std::endl; }
			void on_menu_choices_one() 
			{
				Glib::ustring message;
				if (m_refChoiceOne->get_active())
				{
					message = "Choice1 was selected.";
				}
				else
				{
					message = "Choice1 was deselected.";
				}
				std::cout << message << std::endl;
			}

			void on_menu_choices_two()
			{
				std::cout << "blaaah" << std::endl;
			}

		protected:
			Gtk::Box m_box;
			//Gtk::Grid m_grid;
			Gsv::View m_view;

			Glib::RefPtr<Gtk::UIManager> 	m_refUIManager;
			Glib::RefPtr<Gtk::ActionGroup>	m_refActionGroup;
			Glib::RefPtr<Gtk::RadioAction>	m_refChoiceOne, m_refChoiceTwo;
	};

	HelloWorld::HelloWorld():
		m_box(Gtk::ORIENTATION_VERTICAL),
		m_view(Gsv::Buffer::create())
	{
		set_title("Scam - 3D vector graphics for schemers");
		set_default_size(1280, 720);
		add(m_box);
		
		m_refActionGroup = Gtk::ActionGroup::create();
		m_refActionGroup->add(Gtk::Action::create("FileNewStandard",
				Gtk::Stock::NEW, "_New", "Create a new file"),
			sigc::mem_fun(*this, &HelloWorld::on_menu_file_new_generic));
		m_refActionGroup->add(Gtk::Action::create("FileNewFoo",
				Gtk::Stock::NEW, "New Foo", "Create a new Foo(l)"),
			sigc::mem_fun(*this, &HelloWorld::on_menu_file_new_generic));
		m_refActionGroup->add(Gtk::Action::create("FileNewGoo",
				Gtk::Stock::NEW, "_New Goo", "Create a new Goo(f)"),
			sigc::mem_fun(*this, &HelloWorld::on_menu_file_new_generic));

		m_refActionGroup->add(Gtk::Action::create("FileMenu", "File"));
		m_refActionGroup->add(Gtk::Action::create("FileNew", Gtk::Stock::NEW));
		m_refActionGroup->add(Gtk::Action::create("FileQuit", Gtk::Stock::QUIT),
			sigc::mem_fun(*this, &HelloWorld::on_menu_file_quit));

		m_refActionGroup->add(Gtk::Action::create("EditMenu", "Edit"));
		m_refActionGroup->add(Gtk::Action::create("EditCopy", Gtk::Stock::COPY),
			sigc::mem_fun(*this, &HelloWorld::on_menu_others));
		m_refActionGroup->add(Gtk::Action::create("EditPaste", Gtk::Stock::PASTE),
			sigc::mem_fun(*this, &HelloWorld::on_menu_others));
		m_refActionGroup->add(Gtk::Action::create("EditSomething", "Something"),
			Gtk::AccelKey("<control><alt>S"),
			sigc::mem_fun(*this, &HelloWorld::on_menu_others));

		//Choices menu, to demonstrate Radio items
		m_refActionGroup->add( Gtk::Action::create("ChoicesMenu", "Choices") );
		Gtk::RadioAction::Group group_userlevel;
		m_refChoiceOne = Gtk::RadioAction::create(group_userlevel, "ChoiceOne", "One");
		m_refActionGroup->add(m_refChoiceOne,
		        sigc::mem_fun(*this, &HelloWorld::on_menu_choices_one) );
		m_refChoiceTwo = Gtk::RadioAction::create(group_userlevel, "ChoiceTwo", "Two");
		m_refActionGroup->add(m_refChoiceTwo,
		        sigc::mem_fun(*this, &HelloWorld::on_menu_choices_two) );

		//Help menu:
		m_refActionGroup->add( Gtk::Action::create("HelpMenu", "Help") );
		m_refActionGroup->add( Gtk::Action::create("HelpAbout", Gtk::Stock::HELP),
		        sigc::mem_fun(*this, &HelloWorld::on_menu_others) );
		
		m_refUIManager = Gtk::UIManager::create();
		m_refUIManager->insert_action_group(m_refActionGroup);

		add_accel_group(m_refUIManager->get_accel_group());

		//Layout the actions in a menubar and toolbar:
		Glib::ustring ui_info = 
		      "<ui>"
		      "  <menubar name='MenuBar'>"
		      "    <menu action='FileMenu'>"
		      "      <menu action='FileNew'>"
		      "        <menuitem action='FileNewStandard'/>"
		      "        <menuitem action='FileNewFoo'/>"
		      "        <menuitem action='FileNewGoo'/>"
		      "      </menu>"
		      "      <separator/>"
		      "      <menuitem action='FileQuit'/>"
		      "    </menu>"
		      "    <menu action='EditMenu'>"
		      "      <menuitem action='EditCopy'/>"
		      "      <menuitem action='EditPaste'/>"
		      "      <menuitem action='EditSomething'/>"
		      "    </menu>"
		      "    <menu action='ChoicesMenu'>"
		      "      <menuitem action='ChoiceOne'/>"
		      "      <menuitem action='ChoiceTwo'/>"
		      "    </menu>"
		      "    <menu action='HelpMenu'>"
		      "      <menuitem action='HelpAbout'/>"
		      "    </menu>"
		      "  </menubar>"
		      "  <toolbar  name='ToolBar'>"
		      "    <toolitem action='FileNewStandard'/>"
		      "    <toolitem action='FileQuit'/>"
		      "  </toolbar>"
		      "</ui>";
		
		try
		{
		  m_refUIManager->add_ui_from_string(ui_info);
		}
		catch(const Glib::Error& ex)
		{
		  std::cerr << "building menus failed: " <<  ex.what();
		}
		
		//Get the menubar and toolbar widgets, and add them to a container widget:
		Gtk::Widget* pMenubar = m_refUIManager->get_widget("/MenuBar");
		if(pMenubar)
		  m_box.pack_start(*pMenubar, Gtk::PACK_SHRINK);
		
		Gtk::Widget* pToolbar = m_refUIManager->get_widget("/ToolBar") ;
		if(pToolbar)
		  m_box.pack_start(*pToolbar, Gtk::PACK_SHRINK);

		m_box.pack_start(m_view, Gtk::PACK_EXPAND_WIDGET, 0);
		show_all_children();
	}

	HelloWorld::~HelloWorld() {}

	void command_gui(int argc, char **argv)
	{
		Glib::RefPtr<Gtk::Application> app = Gtk::Application::create(argc, argv, "org.gtkmm.example");	
		HelloWorld helloworld;
		app->run(helloworld);
	}
}

#include "../base/global.hh"
System::Global<Command> _COMMAND_GUI("gui", GUI::command_gui);

