import { getPostByLang } from '@/contents/cms';
import PostList from '@/components/PostList';
import { setRequestLocale } from 'next-intl/server';

interface BlogPageProps {
  params: Promise<{
    page: string
    locale: string
  }>
}

export default async function BlogPage(props: BlogPageProps) {
  const params = await props.params;

  const {
    locale
  } = params;

  setRequestLocale(locale);

  const posts = (await getPostByLang(locale))
    .filter(post => !post.metaData.draft)
    .sort((a, b) => new Date(b.metaData.date).getTime() -
      new Date(a.metaData.date).getTime())

  return (
    <main className="mx-auto w-full max-w-2xl px-6 py-16 lg:py-24">
      <header className="mb-16">
        <h1 className="font-heading text-sm uppercase tracking-[0.2em] text-foreground/40">
          Archive
        </h1>
        <div className="mt-4 h-px bg-gradient-to-r from-foreground/[0.08] via-foreground/[0.08] to-transparent" />
      </header>
      <PostList posts={posts} />
    </main>
  );
}

export async function generateStaticParams() {
  return [{}];
}
 