#pragma once

namespace Scam
{
	class Colour
	{
		double m_r, m_g, m_b;
		double m_h, m_s, m_v;
		double m_a;

		bool m_alpha;

		Colour(double r_, double g_, double b_, double h_, double s_, double v_):
			m_r(r_), m_g(g_), m_b(b_), m_h(h_), m_s(s_), m_v(v_), m_a(1.0), 
			m_alpha(false) {}

		Colour(double r_, double g_, double b_, double h_, double s_, double v_, double a_):
			m_r(r_), m_g(g_), m_b(b_), m_h(h_), m_s(s_), m_v(v_), m_a(a_), 
			m_alpha(true) {}

		public:
			bool alpha() const { return m_alpha; }

			double r() const { return m_r; }
			double g() const { return m_g; }
			double b() const { return m_b; }
			double h() const { return m_h; }
			double s() const { return m_s; }
			double v() const { return m_v; }
			double a() const { return m_a; }

			static Colour RGB(double r_, double g_, double b_);
			static Colour HSV(double h_, double s_, double v_);
			static Colour RGBA(double r_, double g_, double b_, double a_);
			static Colour HSVA(double h_, double s_, double v_, double a_);
	};

	class HSV_gradient
	{
		Colour A, B;

		public:
			HSV_gradient(Colour const &A_, Colour const &B_):
				A(A_), B(B_) {}

			Colour operator()(double t)
			{
				double h = A.h() + (B.h() - A.h()) * t,
				       v = A.v() + (B.v() - A.v()) * t,
				       s = A.s() + (B.s() - A.s()) * t;

				if (A.alpha() or B.alpha())
				{
					double a = A.a() + (B.a() - A.a()) * t;
					return Colour::HSVA(h, s, v, a);
				}
				else
				{
					return Colour::HSV(h, s, v);
				}
			}
	};

	class RGB_gradient
	{
		Colour A, B;

		public:
			RGB_gradient(Colour const &A_, Colour const &B_):
				A(A_), B(B_) {}

			Colour operator()(double t)
			{
				double r = A.r() + (B.r() - A.r()) * t,
				       g = A.g() + (B.g() - A.g()) * t,
				       b = A.b() + (B.b() - A.b()) * t;

				if (A.alpha() or B.alpha())
				{
					double a = A.a() + (B.a() - A.a()) * t;
					return Colour::RGBA(r, g, b, a);
				}
				else
				{
					return Colour::RGB(r, g, b);
				}
			}
	};
}

