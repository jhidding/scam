#include "data.hh"

std::ostream &PLY::operator<<(std::ostream &out, PLY::Data const &data)
{
	if (data.format() == PLY::ASCII)
		data.write_ascii(out);
	else
		data.write_binary(out);

	return out;
}

