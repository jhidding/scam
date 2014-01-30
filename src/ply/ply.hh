#pragma once
#include <string>
#include <iostream>
#include <fstream>
#include <utility>
#include <memory>
#include <vector>
#include <typeinfo>

#include "base.hh"
#include "header.hh"
#include "data.hh"

namespace PLY
{
	class PLY
	{
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
}

