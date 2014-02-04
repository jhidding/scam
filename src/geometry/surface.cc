#include "surface.hh"
#include "polygon.hh"

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

bool Surface::is_below(Segment const &s) const
{
	return is_below(s.first()) and is_below(s.second());
}

bool Surface::is_below(Polygon const &p) const
{
	for (Vertex const &v : p)
		if (not is_below(v)) return false;
	return true;
}

template <typename F1, typename F2>
std::pair<Maybe<Polygon>, Maybe<Polygon>> _split_polygon(
	F1 splitter, F2 is_below, Polygon const &P, bool closed = true)
{
	using return_type = std::pair<Maybe<Polygon>,Maybe<Polygon>>;

	if (P.empty()) return return_type(Nothing, Nothing);

	std::vector<Vertex> V; 
	std::copy(P.vertices().begin(), P.vertices().end(), std::back_inserter(V));

	if (closed)
		V.push_back(V.front());

	Array<Vertex> Q1, Q2;

	auto i = V.begin();
	auto j = i; ++j;
	bool below = is_below(*i), split = false;

	while (j != V.end())
	{
		if (below != is_below(*j))
		{
			Maybe<Vertex> q(splitter(Segment(*i, *j)));

			if (q)
			{
				split = true;
				Q1.push_back(*q);
				Q2.push_back(*q);
				std::swap(Q1, Q2);
			}

			below = not below;
		}
		else
		{
			Q1.push_back(*j);	
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


std::pair<Maybe<Polygon>, Maybe<Polygon>> Surface::split_polygon(
	Polygon const &P) const
{
	return _split_polygon(
		[this] (Segment const &s) -> Maybe<Vertex>
		{
			Maybe<Point> p = this->intersect(s.first(), s.second());
			if (p)
				return Just(Vertex(*p));
			else
				return Nothing;
		},

		[this] (Point const &p)
		{
			return this->is_below(p);
		}, P);
}

std::pair<Maybe<Polygon>, Maybe<Polygon>> Surface::split_polygon(
	ptr<std::unordered_map<Segment,Vertex>> cache, Polygon const &P) const
{
	return _split_polygon(
		[this, cache] (Segment const &s) -> Maybe<Vertex>
		{
			if (cache->count(s) > 0)
				return Just((*cache)[s]);

			Maybe<Point> q = this->intersect(s.first(), s.second());
			if (q)
			{
				Vertex v(*q);
				(*cache)[s] = v;
				return Just(v);
			}

			return Maybe<Vertex>();
		}, 

		[this] (Point const &p)
		{
			return this->is_below(p);
		}, P);
}

std::pair<Maybe<Segment>, Maybe<Segment>> 
Surface::split_segment(Segment const &s) const
{
	using return_type = 
		std::pair<Maybe<Segment>, Maybe<Segment>>;

	bool a = is_below(s.first()), b = is_below(s.second());

	if (a and b)
		return return_type(Just(s), Nothing);

	if ((not a) and (not b))
		return return_type(Nothing, Just(s));

	auto p = intersect(s.first(), s.second());
	if (not p) // shouldn't happen
		return return_type(Just(s), Nothing);

	Vertex v(*p);
	return return_type(
		Just(Segment(s.first(), v)),
		Just(Segment(v, s.second())));
}

