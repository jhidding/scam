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

	class Drawable
	{
		Path		G;
		Plane		P;
		Material	M;

		double		z;

		public:
			Drawable() {}

			Drawable(Path const &G_, Plane const &P_, Material const &M_):
				G(G_), P(P_), M(M_)
			{
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

				if (G.closed())
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
		public:
			virtual Array<Drawable> operator()(ptr<Camera> C) const = 0;
	};

	class VertexObject: public RenderObject
	{
		Array<Vertex>	V;
		Material	M;

		public:
			VertexObject(Array<Vertex> V_, Material const &M_):
				V(V_), M(M_) {}

			Array<Drawable> operator()(ptr<Camera> C) const
			{
				Array<Drawable> A;
				for (Vertex const &v : V)
				{
					Point p = (*C)(v);
					Path G(false); G.push_back(p);
					A.push_back(Drawable(G, Plane(), M));
				}
				return A;
			}
	};

	class PolygonObject: public RenderObject
	{
		Array<Polygon>	P;
		Material	M;

		public:
			PolygonObject() {}

			PolygonObject(Array<Polygon> P_, Material const &M_):
				P(P_), M(M_)
			{}

			size_t size() const { return P.size(); }

			Array<Drawable> operator()(ptr<Camera> C) const
			{
				Array<Drawable> D;
				for (Polygon const &p : P)
				{
					Array<Path> A = (*C)(p);
					for (Path const g : A)
						D.push_back(Drawable(g, (*C)(p.plane()), M));
				}
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

			static ptr<Renderer> PDF(double width, double height, std::string const &filename)
			{
				return ptr<Renderer>(new Renderer(Cairo::PdfSurface::create(filename, width, height)));
			}

			static ptr<Renderer> Image(double width, double height)
			{
				return ptr<Renderer>(new Renderer(Cairo::ImageSurface::create(Cairo::FORMAT_ARGB32, width, height)));
			}

			void write_to_png(std::string const &fn) const
			{
				m_surface->write_to_png(fn);
			}

			void finish()
			{
				m_surface->finish();	
			}

			void apply(std::function<void (Context)> f)
			{
				f(m_context);
			}

			void render(Array<ptr<RenderObject>> Scene, ptr<Camera> C)
			{
				auto drawables = flatmap([C] (ptr<RenderObject> r) { return (*r)(C); }, Scene);
				sort(drawables);
				std::cerr << "rendering " << drawables.size() << " polygons.\n";
				for_each([this] (Drawable const &d) { return d(m_context); }, drawables);
			}
	};
}

