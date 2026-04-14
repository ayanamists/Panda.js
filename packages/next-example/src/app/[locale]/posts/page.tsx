import { getPostByLang } from '@/contents/cms';
import PostList from '@/components/PostList';
import { Suspense } from 'react';
import Loading from '@/components/Loading';

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

  const posts = (await getPostByLang(locale))
    .filter(post => !post.metaData.draft)
    .sort((a, b) => new Date(b.metaData.date).getTime() -
      new Date(a.metaData.date).getTime())

  return (
    <main className="mx-auto w-full max-w-2xl px-6 py-8 lg:py-12">
      <Suspense fallback={<Loading />}>
        <PostList posts={posts} />
      </Suspense>
    </main>
  );
}

export async function generateStaticParams() {
  return [{}];
}
