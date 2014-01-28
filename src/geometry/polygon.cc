#include <functional>
#include <vector>
#include <iterator>
#include <algorithm>
#include "polygon.hh"

using namespace Scam;

size_t Polygon::compute_hash() const
{
	std::vector<size_t> H;
	std::transform(m_vert->begin(), m_vert->end(), std::back_inserter(H), std::hash<Vertex>());

	size_t h = 0;
	for (size_t i : H) h = h*1024 + i;
	return h;
}

