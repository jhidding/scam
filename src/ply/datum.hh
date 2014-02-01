// DatumSeparator {{{3
class Data::DatumSeparator: public Datum
{
	public:
		virtual ByteVector byte_vector() const { throw Exception("No byte representation for separator."); }

		void write_binary(std::ostream &out) const
		{}

		void write_ascii(std::ostream &out) const
		{
			out << " ";
		}

		void read_ascii(std::istream &in) {}

		void read_binary(std::istream &in) {}

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
		virtual ByteVector byte_vector() const { throw Exception("No byte representation for separator."); }

		void write_binary(std::ostream &out) const
		{}

		void write_ascii(std::ostream &out) const
		{
			out << std::endl;
		}

		void read_ascii(std::istream &in) {}

		void read_binary(std::istream &in) {}

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
		virtual ByteVector byte_vector() const 
		{
			std::vector<char> A;
			for (unsigned i = 0; i < sizeof(T); ++i)
				A.push_back(*(reinterpret_cast<char const *>(&m_value) + i));
			return ByteVector(A, PLY::Type<T>::id);
		}

		Scalar() {}
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

		void read_ascii(std::istream &in)
		{
			if (typeid(T) == typeid(uint8_t))
			{
				int a;
				in >> a;
				m_value = a;
			}
			else
			{
				in >> m_value;
			}
		}

		void read_binary(std::istream &in)
		{
			in.read(reinterpret_cast<char *>(&m_value), sizeof(T));
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
		virtual ByteVector byte_vector() const
		{
			std::vector<char> A;
			for (unsigned i = 0; i < sizeof(T)*m_value.size(); ++i)
				A.push_back(*(reinterpret_cast<char const *>(m_value.data()) + i));
			return ByteVector(A, PLY::Type<T>::id);
		}

		List() {}

		template <typename U>
		List(U const &u): m_value(u.begin(), u.end()) {}

		void write_binary(std::ostream &out) const
		{
			length_type length = m_value.size();
			out.write(reinterpret_cast<char *>(&length), sizeof(length_type));
			for (T v : m_value) 
				out.write(reinterpret_cast<char const *>(&v), sizeof(T));
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

		void read_ascii(std::istream &in)
		{
			length_type s;
			in >> s;
			for (length_type i = 0; i < s; ++i)
			{
				T v; in >> v;
				m_value.push_back(v);
			}
		}

		void read_binary(std::istream &in)
		{
			length_type s;
			in.read(reinterpret_cast<char *>(&s), sizeof(length_type));
			m_value.resize(s);
			in.read(reinterpret_cast<char *>(m_value.data()), sizeof(T)*s);
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

