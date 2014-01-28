#include <numeric>
#include <cmath>

#include "colour.hh"
#include "../base/common.hh"

using namespace Scam;

void rgb_to_hsv(double r, double g, double b, double &h, double &s, double &v)
{
	h = 0; s = 0;

	if ((r > g) and (r > b))
	{
		double min_rgb = std::min(g, b);

		if ((r - min_rgb) > 1e-6)
		{
			h = ((g - b) / (r - min_rgb)) / 3.;
			s = (r - min_rgb) / r;
		}

		v = r;
	}
	else if ((g > r) and (g > b))
	{
		double min_rgb = std::min(b, r);

		if ((g - min_rgb) > 1e-6)
		{
			h = ((b - r) / (g - min_rgb) + 1.) / 3.;
			s = (g - min_rgb) / g;
		}

		v = g;
	}
	else
	{
		double min_rgb = std::min(r, g);

		if ((b - min_rgb) > 1e-6)
		{
			h = ((r - g) / (b - min_rgb) + 2.) / 3;
			s = (b - min_rgb) / b;
		}

		v = b;
	}
}

#include <iostream>
void hsv_to_rgb(double &r, double &g, double &b, double h, double s, double v)
{
	int Hi = modulus(static_cast<int>(floor(h * 6)), 6);
	double f = (modulus(h, 1.0) * 6) - Hi;

	double k = (Hi % 2 == 0 ? 1 - f : f),
	       l = v * (1 - s),
	       m = v * (1 - k*s);

	switch (Hi)
	{
		case 0: r = v; g = m; b = l; break;
		case 1: r = m; g = v; b = l; break;
		case 2: r = l; g = v; b = m; break;
		case 3: r = l; g = m; b = v; break;
		case 4: r = m; g = l; b = v; break;
		case 5: r = v; g = l; b = m; break;
	}
}

Colour Colour::RGB(double r_, double g_, double b_)
{
	double h_, s_, v_;
	rgb_to_hsv(r_, g_, b_, h_, s_, v_);
	return Colour(r_, g_, b_, h_, s_, v_);
}

Colour Colour::HSV(double h_, double s_, double v_)
{
	double r_, g_, b_;
	hsv_to_rgb(r_, g_, b_, h_, s_, v_);
	return Colour(r_, g_, b_, h_, s_, v_);
}

Colour Colour::RGBA(double r_, double g_, double b_, double a_)
{
	double h_, s_, v_;
	rgb_to_hsv(r_, g_, b_, h_, s_, v_);
	return Colour(r_, g_, b_, h_, s_, v_, a_);
}

Colour Colour::HSVA(double h_, double s_, double v_, double a_)
{
	double r_, g_, b_;
	hsv_to_rgb(r_, g_, b_, h_, s_, v_);
	return Colour(r_, g_, b_, h_, s_, v_, a_);
}

