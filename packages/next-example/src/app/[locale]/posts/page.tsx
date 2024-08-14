import { getPostByLang } from '@/contents/cms';
import PostList from '@/components/PostList';
import { Suspense } from 'react';
import Loading from '@/components/Loading';

interface BlogPageProps {
  params: {
    page: string
    locale: string
  }
}

export default async function BlogPage({
  params: { locale } 
}: BlogPageProps) {
  const posts = (await getPostByLang(locale))
    .filter(post => !post.metaData.draft)
    .sort((a, b) => new Date(b.metaData.date).getTime() -
      new Date(a.metaData.date).getTime())

  return (
    <main className="container mx-auto relative w-full px-6
    md:max-w-4xl md:mx-auto lg:max-w-5xl lg:pt-2 lg:pb-28">
      <Suspense fallback={<Loading />}>
        <PostList posts={posts} />
      </Suspense>
    </main>
  );
}

// @ts-ignore
export async function generateStaticParams({ params }) {
  return [ { page: '1' } ]
}
