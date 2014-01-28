#include "surface.hh"

using namespace Scam;

double Plane::distance(Point const &p) const
{
	return m_normal * (p - m_origin);
}

Maybe<Point> Plane::intersect(Point const &a, Point const &b) const
{
	if (distance(a) * distance(b) >= 0) return Nothing;

	Vector v = b - a;
	double t = m_normal * (m_origin - a) / (m_normal * v);
	return Just(a + v*t);
}

double Sphere::distance(Point const &p) const
{
	return (p - m_origin).norm() - m_radius;
}

Maybe<Point> Sphere::intersect(Point const &a, Point const &b) const
{
	if (distance(a) * distance(b) >= 0) return Nothing;

	Vector l = b - a, m = a - m_origin;
	double l2 = l.sqr(), lm = l*m;
	double D = lm*lm - (l2 * (m.sqr() - m_radius*m_radius));

	if (D < 0) return Nothing; // shouldn't really happen

	double sol_m = (- lm - sqrt(D)) / l2,
	       sol_p = (- lm + sqrt(D)) / l2;

	if ((sol_m >= 0) and (sol_m <= 1.0))
		return Just(a + l*sol_m);

	if ((sol_p >= 0) and (sol_p <= 1.0))
		return Just(a + l*sol_p);

	return Nothing; // shouldn't really happen
}

bool Surface::is_below(Point const &a) const
{
	return distance(a) < 0;
}

template <typename at, typename rt>
rt cache_or_compute(ptr<std::unordered_map<at, rt>> cache, std::function<rt (at const &)> f, at const &a)
{
	auto k = cache->find(a);
	if (k != cache->end()) return *k;

	rt q = f(a);
	(*cache)[a] = q;

	return q;
}

std::pair<Maybe<Polygon>, Maybe<Polygon>> Surface::split_polygon(
	ptr<std::unordered_map<Segment,Point>> cache, ptr<Polygon> P) const
{
	using return_type = std::pair<Maybe<Point>,Maybe<Point>>;

	if (P->empty()) return return_type(Nothing, Nothing);

	std::vector<Vertex> V; 
	std::copy(P->vertices()->begin(), P->vertices->end(), std::back_inserter(V));
	V.push_back(V.front());

	ptr<std::vector<Vertex>> Q1(new std::vector<Vertex>), 
				 Q2(new std::vector<Vertex>);

	auto i = V.begin();
	auto j = i; ++j;
	bool below = is_below(*i), split = false;

	while (j != V.end())
	{
		if (below != is_below(*j))
		{
			auto q = cache_or_compute<Segment, Point>(cache, [] (Segment const &s) 
			{ 
				return intersect(s.first(), s.second()); 
			}, Segment(*i, *j));

			if (q)
			{
				split = true;
				Vertex n(*q);
				Q1->push_back(n);
				Q2->push_back(n);
				std::swap(Q1, Q2);
			}
		}
		else
		{
			Q1->push_back(*j);
			
			++i; ++j;
		}
	}

	if (not split)
	{
		if (below)
			return return_type(Just(P), Nothing);
		else
			return return_type(Nothing, Just(P));
	}

	if (below)
		return return_type(Just(Polygon(Q1)), Just(Polygon(Q2)));
	else
		return return_type(Just(Polygon(Q2)), Just(Polygon(Q1)));
}

