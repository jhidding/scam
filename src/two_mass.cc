#include "two_mass.hh"
#include <sstream>
#include <iostream>
#include <string>

using namespace Scam;
using namespace TwoMass;

std::istream &TwoMass::operator>>(std::istream &in, Galaxy &G)
{
	std::string line;
	do {
		std::getline(in, line);
	} while (line[0] == '#');

	std::istringstream ss(line);
	ss >> G.ID   >> G.RA    >> G.DEC   >> G.l    >> G.b
	   >> G.k_c  >> G.h_c   >> G.j_c   >> G.k_tc >> G.h_tc   >> G.j_tc
	   >> G.e_k  >> G.e_h   >> G.e_j   >> G.e_kt >> G.e_ht   >> G.e_jt
	   >> G.e_bv >> G.r_iso >> G.r_ext >> G.ba   >> G.flags  >> G.type
	   >> G.ts   >> G.v     >> G.e_v   >> G.c    >> G.vsrc   >> G.CAT_ID;

	return in;
}

TwoMass::Spherical_rotation::Spherical_rotation(double pole_ra_, double pole_dec_, double angle_):
	pole_ra(pole_ra_), pole_dec(pole_dec_), angle(angle_) {}

void TwoMass::Spherical_rotation::operator()(double ra, double dec, double &u, double &v) const
{
	v = asin(cos(dec) * cos(pole_dec) * cos(ra - pole_ra) 
		+ sin(dec) * sin(pole_dec));

	u = atan2(sin(dec) - sin(v) * sin(pole_dec),
		cos(dec) * sin(ra - pole_ra) * cos(pole_dec))
		+ angle;
}

Spherical_rotation TwoMass::Spherical_rotation::inv() const
{
	double pra, pdec; (*this)(0, M_PI/2, pra, pdec);
	double a, b; (*this)(0, 0, a, b);
	double ang, dummy; Spherical_rotation(pra, pdec, 0)(a, b, ang, dummy);
	return Spherical_rotation(pra, pdec, -ang);
}

Spherical_rotation TwoMass::Spherical_rotation::operator*(Spherical_rotation const &o) const
{
	double pra, pdec;  
	o.inv()(pole_ra, pole_dec, pra, pdec);
	double phi, dummy; 
	Spherical_rotation(pra, pdec, 0).inv()(0, 0, phi, dummy);
	o(phi, dummy, phi, dummy);
	(*this)(phi, dummy, phi, dummy);
	return Spherical_rotation(pra, pdec, phi);
}

Spherical_rotation TwoMass::eq_to_ga(radians(192.859508), radians(27.128336), radians(122.932 - 90.0));
Spherical_rotation TwoMass::ga_to_eq = TwoMass::eq_to_ga.inv();

Spherical_rotation TwoMass::ga_to_sg(radians(47.37), radians(6.32), 0.0);
Spherical_rotation TwoMass::sg_to_ga = TwoMass::ga_to_sg.inv();

Spherical_rotation TwoMass::eq_to_sg = TwoMass::ga_to_sg * TwoMass::eq_to_ga;
Spherical_rotation TwoMass::sg_to_eq = TwoMass::eq_to_sg.inv();

Vector TwoMass::spherical_to_cartesian(double ra, double dec, double r)
{
	double x = r * cos(dec) * cos(ra),
	       y = r * cos(dec) * sin(ra),
	       z = r * sin(dec);

	return Scam::Vector(x, y, z);
}

