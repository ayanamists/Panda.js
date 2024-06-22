import PostContent from '@/contents';
import { getPostByLang, getPostById } from '@/contents/cms';
import { unstable_setRequestLocale } from 'next-intl/server';

interface PageProps {
  params: {
    slug: string
    locale: string
  }
}

export default async function Page({ params }: PageProps) {
  unstable_setRequestLocale(params.locale);
  const id = params.slug;
  const lang = params.locale;
  const post = await getPostById(id, lang);
  if (post == null) {
    throw new Error(`Post not found: id: ${id}, lang: ${lang}`);
  }

  const path = post.metaData.fullName;
  const heading = post.metaData.title;
  return (<div>
    <h1 className="text-4xl font-bold mb-2">{heading}</h1>
    <div className="text-sm text-gray-500 mb-5">
    {new Date(post.metaData.date).toLocaleDateString()}
    </div>
    <PostContent name={path} />
  </div>);
}


export async function generateStaticParams({ params }: {
  params: { locale: string }
}) {
  const locale = params.locale;
  const posts = await getPostByLang(locale);
  return posts
    .map(post => ({
      slug: post.id,
    }));
}
