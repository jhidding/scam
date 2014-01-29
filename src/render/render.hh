#pragma once

#include <cairomm/cairomm.h>

namespace Scam
{
	using Context = Cairo::RefPtr<Cairo::Context>;
	using Material = std::function<std::function<void (Context)>(Plane const &)>;

	class Collection
	{
		std::vector<ptr<Polygon>> 	P;
		ptr<Material>			M;

		public:
	};

	class Drawable
	{
		ptr<Polygon> 	P;
		Material	M;

		public:
			void apply(Context cx)
			{
				Point  p = head(*P);
				cx->move_to(p.x(), p.y());

				for (Point const &p : tail(*P))
					cx->line_to(p.x(), p.y());

				cx->close_path();

				(*M)(P->plane()).apply(cx);
			}

			bool operator<(Drawable const &o) const
			{
				return head(*P).z() < head(*o.P).z();
			}
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

