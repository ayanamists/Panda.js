"use client";

import { Post } from '@/contents/cms';
import Link from 'next/link';

export default function PostCard({ post }: { post: Post }) {
  return (
    <Link href={`/${post.metaData.language}/posts/${post.id}`}
      className='text-lg whitespace-normal text-primary'
      aria-label='Post'
    >
      {post.metaData.title}
    </Link>
  );
}
