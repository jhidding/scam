#pragma once

#include <iostream>
#include <string>
#include <vector>
#include <memory>
#include <map>

#include "base.hh"

namespace PLY
{
	class Property
	{
		std::string m_name;

		public:
			Property(std::string const &name_): m_name(name_) {}
			std::string const &name() const { return m_name; }
			virtual std::string type_expression() const = 0;
			virtual ptr<Property> copy() const = 0;
	};

	class Header
	{
		public:
			template <typename T> 
			class Scalar;

			template <typename T, typename length_type = uint8_t>
			class List;

			class Element
			{
				std::string 			m_name;
				size_t				m_count;
				std::vector<ptr<Property>> 	m_properties;

				public:
					Element(std::string const &name_): 
						m_name(name_), m_count(0) {}

					Element(std::string const &name_, size_t count_):
						m_name(name_), m_count(count_) {}

					std::string const &name() const { return m_name; }
					size_t count() const { return m_count; }

					void add_property(Property const &property)
					{
						m_properties.push_back(property.copy());
					}

					std::vector<ptr<Property>> const &properties() const
					{
						return m_properties;
					}

					void add_item()
					{
						++m_count;
					}
			};

			typedef std::string Comment;

		private:
			mutable Format			m_format;
			std::vector<Comment> 		m_comments;
			std::vector<ptr<Element>>	m_elements;

		public:
			Format format() const 
				{ return m_format; }
			std::vector<Comment> const &comments() const 
				{ return m_comments; }
			std::vector<ptr<Element>> const &elements() const 
				{ return m_elements; }

			void set_format(Format format_) const
			{
				m_format = format_;
			}

			void add_comment(std::string const &comment)
			{
				m_comments.push_back(comment);
			}

			void add_element(std::string const &name)
			{
				m_elements.push_back(ptr<Element>(new Element(name)));
			}

			void add_element(std::string const &name, size_t n)
			{
				m_elements.push_back(ptr<Element>(new Element(name, n)));
			}

			void add_property(Property const &property)
			{
				m_elements.back()->add_property(property);
			}

			void add_item()
			{
				m_elements.back()->add_item();
			}
	};

	template <typename T>
	class Header::Scalar: public Property
	{
		public:
			using Property::Property;

			std::string type_expression() const
			{ return Type<T>::name; }

			virtual ptr<Property> copy() const
			{
				return ptr<Property>(
					new Scalar<T>(name()));
			};
	};

	template <typename T, typename length_type>
	class Header::List: public Property
	{
		public:
			using Property::Property;

			std::string type_expression() const
			{ return "list " + Type<length_type>::name + " " 
				+ Type<T>::name; }

			virtual ptr<Property> copy() const
			{
				return ptr<Property>(
					new List<T, length_type>(name()));
			};
	};

	template <typename T>
	Header::Scalar<T> scalar_type(std::string const &name)
	{ return Header::Scalar<T>(name); }

	template <typename T, typename length_type = uint8_t>
	Header::List<T, length_type> list_type(std::string const &name)
	{ return Header::List<T, length_type>(name); }

	extern std::map<std::string, std::function<ptr<Property> (std::string const &name)>> make_scalar_type;
	extern std::map<std::string, std::function<ptr<Property> (std::string const &name)>> make_list_type;

	extern std::ostream &operator<<(std::ostream &out, PLY::Property const &property);
	extern std::ostream &operator<<(std::ostream &out, PLY::Header::Element const &element);
	extern std::ostream &operator<<(std::ostream &out, PLY::Header const &header);
}

