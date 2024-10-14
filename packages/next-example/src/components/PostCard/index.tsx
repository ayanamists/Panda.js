"use client";

import { Post } from '@/contents/cms';

export default function PostCard({ post }: { post: Post }) {
  return (
    <span
      className='text-lg whitespace-normal text-primary'
      aria-label='Post'
    >
      {post.metaData.title}
    </span>
  );
}
