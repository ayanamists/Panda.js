"use client";

import { Post } from '@/contents/cms';
import { Card, CardBody, CardFooter } from "@nextui-org/card";
import { Link } from '@nextui-org/link';

export default function PostCard({ post }: { post: Post }) {
  return (
    <Card shadow='none' disableAnimation={true} isHoverable={false}
      disableRipple={true}
    >
      <CardBody>
        <Link href={`/${post.metaData.language}/posts/${post.id}`}
          className='text-lg whitespace-normal'
          aria-label='Post'
        >
          {post.metaData.title}
        </Link>
      </CardBody>
      <CardFooter className="flex flex-row justify-between">
        <p className="text-md">{new Date(post.metaData.date).toLocaleDateString()}</p>
        <p className="text-small text-default-500">{post.metaData.categories.join(" ")}</p>
      </CardFooter>
    </Card>
  );
}
