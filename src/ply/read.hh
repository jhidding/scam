#pragma once
#include "ply.hh"
#include "base.hh"
#include <fstream>
#include <stdexcept>
#include <sstream>

#include "../base/common.hh"

namespace PLY
{
	extern Format read_header(std::istream &fi, std::shared_ptr<PLY> ply) throw (Exception);

	class Item: public std::map<std::string, ByteVector>
	{
		public:
			template <typename T> T get(std::string const &q) const
			{ return find(q)->second.as<T>(); }
			template <typename T> std::vector<T> get_vector(std::string const &q) const
			{ return find(q)->second.as_vector<T>(); }
	};

	using Block = Scam::Array<Item>;

	extern Block read_element(std::istream &fi, Header::Element const &e, Format format);
}

