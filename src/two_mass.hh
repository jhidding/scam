#pragma once
#include "base/common.hh"
#include "geometry/vector.hh"

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

}

