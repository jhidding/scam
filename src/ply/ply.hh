#pragma once
#include <string>
#include <iostream>
#include <fstream>
#include <utility>
#include <memory>
#include <vector>
#include <typeinfo>
#include <stdexcept>

#include "base.hh"
#include "header.hh"
#include "data.hh"

namespace PLY
{
	class Exception: public std::exception
	{
		std::string msg;

		public:
			Exception(std::string const &msg_): msg(msg_) {}
			char const *what() const throw () { return msg.c_str(); }
			~Exception() throw () {}
	};

	class PLY
	{
		friend std::shared_ptr<PLY> read(std::string const &) throw (Exception);

		private:
			Header 			m_header;
			Data			m_data;

		public:
			template <typename ...Args>
			void add_element(std::string const &name, Args &&...properties)
			{
				m_header.add_element(name);
				add_properties(std::forward<Args>(properties)...);
			}

			void add_element_n(std::string const &name, size_t count)
			{
				m_header.add_element(name, count);
			}

			void add_property(Property const &prop)
			{
				m_header.add_property(prop);
			}

			void add_comment(std::string const &comment)
			{
				m_header.add_comment(comment);
			}

			void put_data(Datum const &datum)
			{
				m_data.put_datum(datum);
				m_data.put_datum(Data::ItemSeparator());
				m_header.add_item();
			}

			template <typename ...Args>
			void put_data(Datum const &datum, Args &&...args)
			{
				m_data.put_datum(datum);
				m_data.put_datum(Data::DatumSeparator());
				put_data(std::forward<Args>(args)...);
			}

			void write(std::string const &filename, Format format = ASCII) const;

			void print_header(std::ostream &out, Format format = ASCII) const;

			Header::Element const &operator[](std::string const &name) const
			{
				return m_header[name];
			}

		private:
			void add_properties(Property const &prop)
			{
				m_header.add_property(prop);
			}

			template <typename ...Args>
			void add_properties(Property const &prop, Args &&...args)
			{
				m_header.add_property(prop);
				add_properties(std::forward<Args>(args)...);
			}
	};
	
	extern std::shared_ptr<PLY> read(std::string const &filename) throw (Exception);
}

