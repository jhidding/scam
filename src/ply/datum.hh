// DatumSeparator {{{3
class Data::DatumSeparator: public Datum
{
	public:
		void write_binary(std::ostream &out) const
		{}

		void write_ascii(std::ostream &out) const
		{
			out << " ";
		}

		virtual ptr<Datum> copy() const
		{
			return ptr<Datum>(new DatumSeparator());
		}
};
// }}}3

// ItemSeparator {{{3
class Data::ItemSeparator: public Datum
{
	public:
		void write_binary(std::ostream &out) const
		{}

		void write_ascii(std::ostream &out) const
		{
			out << std::endl;
		}

		virtual ptr<Datum> copy() const
		{
			return ptr<Datum>(new ItemSeparator());
		}
};
// }}}3

// Scalar {{{3
template <typename T>
class Data::Scalar: public Datum
{
	T	m_value;

	public:
		Scalar(T t): m_value(t) {}

		void write_binary(std::ostream &out) const
		{
			out.write(reinterpret_cast<char const *>(&m_value), sizeof(T));
		}

		void write_ascii(std::ostream &out) const
		{
			if (typeid(T) == typeid(uint8_t))
				out << int(m_value);
			else
				out << m_value;
		}

		virtual ptr<Datum> copy() const
		{
			return ptr<Datum>(new Scalar<T>(m_value));
		}
};
// }}}3

// List {{{3
template <typename T, typename length_type>
class Data::List: public Datum
{
	std::vector<T> m_value;

	public:
		template <typename U>
		List(U const &u): m_value(u.begin(), u.end()) {}

		void write_binary(std::ostream &out) const
		{
			length_type length = m_value.size();
			out.write(reinterpret_cast<char *>(&length), sizeof(length_type));
			for (T v : m_value) 
				out.write(reinterpret_cast<char *>(&v), sizeof(T));
		}

		void write_ascii(std::ostream &out) const
		{
			out << m_value.size();
			for (T v : m_value)
				if (typeid(T) == typeid(uint8_t))
					out << " " << int(v);
				else
					out << " " << v;
		}

		virtual ptr<Datum> copy() const
		{
			return ptr<Datum>(new List<T, length_type>(m_value));
		}
};
// }}}3

template <typename T>
Data::Scalar<T> scalar(T const &v)
{ return Data::Scalar<T>(v); }

template <typename T, typename Seq>
Data::List<T> list(Seq const &v)
{ return Data::List<T>(v); }

