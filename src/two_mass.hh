#pragma once
#include <iostream>
#include <fstream>
#include "base/common.hh"
#include "geometry/vector.hh"
#include "geometry/point.hh"
#include "geometry/vertex.hh"

namespace Abell
{
	struct Cluster
	{
		std::string 	A_ID;
		double  	ra_b1950_h, ra_b1950_m, dec_b1950_d, dec_b1950_m;
		std::string 	BM_TYPE;
		int		count;
		double		l, b, z;
		int		rich, dclass;
		double		m10;
		double		ra_h, ra_m, ra_s, dec_d, dec_m, dec_s;
		double		ox, oy;
		std::string	name;
	};

	extern std::istream &operator>>(std::istream &in, Cluster &C);
}

namespace TwoMass
{
	inline double radians(double deg) { return (deg / 180.) * M_PI; }
	inline double degrees(double rad) { return (rad / M_PI) * 180.; }

	struct Galaxy
	{
		std::string ID;			// 2MASS identifier

		double  RA, DEC, l, b;		// coordinates equatorial (J2000) and galacic in degrees

		double  k_c,  h_c,  j_c, 	// isophotal magnitudes
			k_tc, h_tc, j_tc, 	// total magnitudes
			e_k, e_h, e_j, 		// uncertainty in isophotal magnitudes
			e_kt, e_ht, e_jt, 	// uncertainty in total magnitudes
			e_bv;			// dust extinction

		double  r_iso, r_ext, ba;	// radii

		std::string flags, type, ts;	// flags and classification

		double  v, e_v;			// velocity and error

		std::string c, vsrc, CAT_ID;	// source info
	};

	extern std::istream &operator>>(std::istream &in, Galaxy &G);

	extern Scam::Vector spherical_to_cartesian(double ra, double dec, double r = 1.0);

	class Spherical_rotation
	{
		double pole_ra, pole_dec, angle;

		public:
			Spherical_rotation(double pole_ra_, double pole_dec_, double angle_);
			void operator()(double ra, double dec, double &u, double &v) const;

			// composition of rotations
			Spherical_rotation operator*(Spherical_rotation const &o) const;

			// inversion
			//Spherical_rotation operator~() const; // same as inv

			Spherical_rotation inv() const;
	};

	extern Spherical_rotation 
		eq_to_ga, ga_to_eq, 
		ga_to_sg, sg_to_ga, 
		eq_to_sg, sg_to_eq;

	inline Scam::Array<Scam::Vertex> read_2mass(std::string const &fn, 
		Spherical_rotation const &sph = ga_to_sg, bool offset = true)
	{
		std::cerr << "Reading 2MASS redshift survey data ... ";
		Scam::Array<Scam::Vertex> A;
		std::ifstream fi(fn);
		while (!fi.eof())
		{
			Galaxy G; fi >> G;
			double sg_ra, sg_dec;
			sph(radians(G.l), radians(G.b), sg_ra, sg_dec);
			auto x = spherical_to_cartesian(sg_ra, sg_dec, G.v / 100.);
			Scam::Vertex v((offset ? Scam::Point(90,90,90) + x : Scam::Point(0,0,0) + x));
			v.set_info("magnitude", G.k_tc);
			A.push_back(v);
		}
		std::cerr << "Ok\n";
		return A;
	}
}

