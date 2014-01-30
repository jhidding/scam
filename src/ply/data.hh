#include <iostream>
#include <string>
#include <vector>
#include <memory>

#include "base.hh"

namespace PLY
{
	class Datum
	{
		public:
			virtual void write_binary(std::ostream &out) const = 0;
			virtual void write_ascii(std::ostream &out) const = 0;
			virtual ptr<Datum> copy() const = 0;
	};

	// Data {{{2
	class Data
	{
		public:
			class DatumSeparator;
			class ItemSeparator;

			template <typename T>
			class Scalar;

			template <typename T, typename length_type = uint8_t>
			class List;

		private:
			mutable Format			m_format;
			std::vector<ptr<Datum>> 	m_data;

		public:
			void set_format(Format format_) const
			{
				m_format = format_;
			}

			Format format() const
			{
				return m_format;
			}

			void put_datum(Datum const &datum) 
			{ 
				m_data.push_back(datum.copy());
			}
			
			/*
			void put_datum_separator() 
			{ 
				m_data.push_back(ptr<Datum>(
					new DatumSeparator()));
			}

			void put_item_separator() 
			{ 
				m_data.push_back(ptr<Datum>(
					new ItemSeparator()));
			}
			*/

			void write_binary(std::ostream &out) const
			{
				for (auto &p_datum : m_data)
					p_datum->write_binary(out);
			}

			void write_ascii(std::ostream &out) const
			{
				for (auto &p_datum : m_data)
					p_datum->write_ascii(out);
			}
	};

	#include "datum.hh"

	extern std::ostream &operator<<(std::ostream &out, Data const &data);
	// }}}2
}


