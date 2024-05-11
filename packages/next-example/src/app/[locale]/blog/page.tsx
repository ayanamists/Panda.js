import { getAllPosts } from '@/contents/cms';
import PostList from '@/components/PostList';

interface BlogPageProps {
  params: {
    page: string
    locale: string
  }
}

export default function BlogPage({ 
  params: { locale } 
}: BlogPageProps) {
  const posts = getAllPosts()
    .filter(post => post.metaData.language === locale && !post.metaData.draft)
    .sort((a, b) => b.metaData.date.getTime() - a.metaData.date.getTime())

  return (
    <main className="container mx-auto relative w-full px-6 py-12
    md:max-w-4xl md:mx-auto lg:max-w-5xl lg:pt-16 lg:pb-28">
      <PostList posts={posts} />
    </main>
  );
}

// @ts-ignore
export async function generateStaticParams({ params }) {
  return [ { page: '1' } ]
}