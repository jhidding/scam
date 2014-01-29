#pragma once

#include <cairomm/cairomm.h>

namespace Scam
{
	using Context = Cairo::RefPtr<Cairo::Context>;
	using Material = std::function<std::function<void (Context)>(Plane const &)>;
	using Path = std::vector<Point>;

	class Drawable
	{
		Path 		G;
		Plane		P;
		Material	M;

		public:
			Drawable(Camera const &C, Polygon const &P_, Material const &M_):
				P(P_.plane()), M(M_)
			{
				std::transform(P_.begin(), P_.end(),
					std::back_inserter(G), C);
			}

			void apply(Context cx)
			{
				Point  p = head(*G);
				cx->move_to(p.x(), p.y());

				for (Point const &p : tail(*G))
					cx->line_to(p.x(), p.y());

				cx->close_path();

				M(P)(cx);
			}

			bool operator<(Drawable const &o) const
			{
				return head(*P).z() < head(*o.P).z();
			}
	};

	class Collection
	{
		std::vector<ptr<Polygon>>	P;
		ptr<Material>			M;

		public:
			
	};

	class Scene
	{
		std::vector<ptr<Collection>>	C;

		public:
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
				return make_ptr<Renderer>(Cairo::SvgSurface::create(filename, width, height));
			}

			void finish()
			{
				m_surface->finish();	
			}

			void operator()(std::function<void (Cairo::RefPtr<Cairo::Context>)> f)
			{
				f(m_context);
			}

			void render(Camera const &C, Scene const &S)
			{

			}
	};
}

