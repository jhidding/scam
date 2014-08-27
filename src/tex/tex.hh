#pragma once
#include <iostream>
#include <sstream>
#include <fstream>
#include <cstdlib>
#include <iomanip>

#include <unistd.h>
#include <librsvg/rsvg.h>

#include "../render/render.hh"

namespace Scam
{
	class SvgFile
	{
		RsvgHandle 		*m_handle;
		RsvgDimensionData	m_dimension;
		bool			m_ok;

		public:
			SvgFile(std::string const &fn);
			bool ok() const;
			virtual ~SvgFile();
			double width() const; 
			double height() const;
			void render(Context cx) const;
	};

	class TeX: public std::ostringstream
	{
		std::string	preamble;
		std::string	fn_prefix;

		void make_svg() const;

		public:
			TeX(std::string const preamble_ = "", unsigned code = rand());
			void reset();
			ptr<SvgFile> svg() const;
			virtual ~TeX();
	};
}

