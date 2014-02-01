#pragma once

#include <cairomm/cairomm.h>
#include <algorithm>
#include <vector>

#include "../base/common.hh"
#include "../base/map.hh"
#include "camera.hh"

namespace Scam
{
	using Context = Cairo::RefPtr<Cairo::Context>;
	using Material = std::function<void (Plane const &, Context)>;
	using Path = Array<Point>;

	class Drawable
	{
		Path 		G;
		Plane		P;
		Material	M;

		bool		closed;
		double		z;

		public:
			Drawable() {}

			Drawable(Polygon const &P_, Material const &M_, Camera const &C):
				P(C(P_.plane())), M(M_), closed(true)
			{
				std::transform(P_.begin(), P_.end(),
					std::back_inserter(G), C);

				auto z_values = lazy_map(G, [] (Point const &x) { return x.z(); });
				z = *std::max_element(z_values.begin(), z_values.end());
			}

			void operator()(Context cx) const
			{
				cx->save();

				Point  p = head(G);
				cx->move_to(p.x(), p.y());

				for (Point const &p : tail(G))
					cx->line_to(p.x(), p.y());

				if (closed)
					cx->close_path();

				M(P, cx);

				cx->restore();
			}

			bool operator<(Drawable const &o) const
			{
				return z > o.z;
			}
	};

	class RenderObject
	{
		Array<Polygon>	P;
		Material	M;

		public:
			RenderObject() {}

			RenderObject(Array<Polygon> P_, Material const &M_):
				P(P_), M(M_)
			{}

			size_t size() const { return P.size(); }

			Array<Drawable> operator()(Camera const &C) const
			{
				Array<Drawable> D;
				for (Polygon const &p : P)
					D.push_back(Drawable(p, M, C));
				return D;
			}
	};

	class Renderer
	{
		Cairo::RefPtr<Cairo::Surface> m_surface;
		Cairo::RefPtr<Cairo::Context> m_context;

		private:
			Renderer(Cairo::RefPtr<Cairo::Surface> surface_):
				m_surface(surface_)
			{
				m_context = Cairo::Context::create(m_surface);
			}

		public:
			static ptr<Renderer> SVG(double width, double height, std::string const &filename)
			{
				return ptr<Renderer>(new Renderer(Cairo::SvgSurface::create(filename, width, height)));
			}

			void finish()
			{
				m_surface->finish();	
			}

			void apply(std::function<void (Context)> f)
			{
				f(m_context);
			}

			void render(Array<RenderObject> Scene, Camera const &C)
			{
				auto drawables = flatmap([&C] (RenderObject const &r) { return r(C); }, Scene);
				sort(drawables);
				std::cerr << "rendering " << drawables.size() << " polygons.\n";
				for_each([this] (Drawable const &d) { return d(m_context); }, drawables);
			}
	};
}

