"use client";

import { Listbox, ListboxItem } from '@nextui-org/listbox';
import PostCard from '@/components/PostCard';
import { Post } from '@/contents/cms';
import { useRouter } from '@/navigation';
import { formatDate } from '@/utils';

export default function PostList({ posts }: { posts: Post[] }) {
  const router = useRouter();
  return (
    <Listbox aria-label='Post List' className='mt-6'>
      {posts.map(post => (
        <ListboxItem key={post.id}
          textValue={post.metaData.title}
          onClick={() => {
            router.push(`/posts/${post.id}`);
          }}
        >
          <div className='sm:flex items-center'>
            <div className='w-24'>
              <time className="text-sm font-semibold text-gray-500 font-mono">
                {formatDate(post.metaData.date)}
              </time>
            </div>
            <div>
              <PostCard post={post} />
            </div>
          </div>
        </ListboxItem>
      ))}
    </Listbox>
  );
}

