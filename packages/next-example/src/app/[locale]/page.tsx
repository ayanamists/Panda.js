import { FaGithub } from "react-icons/fa";
import { FaTwitter } from "react-icons/fa";
import { FaHeart } from "react-icons/fa";
import { SiDouban } from "react-icons/si";
import { SiZhihu } from "react-icons/si";
import { SiMeituan } from "react-icons/si";

import React from 'react';
import Link from 'next/link';

function SmallTitle({ children }: { children: React.ReactNode }) {
  return (<h2 className='text-xl font-bold font-heading'>
    {children}
  </h2>)
}

function ProfileBadge({ href, icon }: {
  href: string,
  icon: React.ReactNode
}) {
  return (
    <div className='hover:text-primary'>
      <Link href={href}>
        {icon}
      </Link>
    </div>)
}

function HomePage() {
  return (
    <div className='flex min-h-[60vh] ml-6 mr-6'>
      <div className='m-auto flex gap-5 md:gap-10 flex-wrap
        w-full md:w-3xl md:mx-auto lg:w-4xl justify-center'>
        <div className='w-full md:w-48 lg:w-30 flex flex-col'>
          <div className='text-3xl text-center font-mainpage'>
            <ruby>李晨曦<rt>liˇ ʈʂʰənˊ ɕiˉ</rt></ruby>
            <div className='text-sm'>Li Chenxi</div>
          </div>
          <div className='flex mt-2 gap-1 text-2xl justify-center'>
            <ProfileBadge href={"https://github.com/ayanamists"} icon={<FaGithub />} />
            <ProfileBadge href={"https://x.com/chezchenxi"} icon={<FaTwitter />} />
            <ProfileBadge href={"https://www.douban.com/people/191397140"} icon={<SiDouban />} />
            <ProfileBadge href={"https://www.zhihu.com/people/woodwardchenxi"} icon={<SiZhihu />} />
          </div>
        </div>

        <div className='md:max-w-xl lg:max-w-2xl font-mainpage'>
          <div className='mb-5'>
            <SmallTitle>About</SmallTitle>
            <span>A Programmer. </span>
            <span className="md:whitespace-nowrap">
              {`B.Eng., Sun Yat-sen University; M.Eng., Nanjing University.`}
            </span>
            <span className="inline-flex items-center md:whitespace-nowrap">
              <span>Now at Meituan</span>
              <SiMeituan className="w-6 h-6 ml-1" />
              <span>.</span>
            </span>
            <div className="mt-2 flex items-center text-rose-500">
              <FaHeart className="w-3.5 h-3.5 mr-1.5 animate-pulse" />
              <span className="font-serif italic font-medium tracking-wide">In love.</span>
            </div>
          </div>

          <div className='flex lg:gap-20 md:gap-20 gap-5 flex-wrap mb-5'>
            <div>
              <SmallTitle>Interest Fields</SmallTitle>
              <ul>
                <li>Functional Programming</li>
                <li>Programming Languages (Theory)</li>
                <li>Program Analysis</li>
                <li>Mathematical Logic</li>
              </ul>
            </div>

            <div>
              <SmallTitle>Dev Skills</SmallTitle>
              <ul>
                <li>Web Development</li>
                <li>Compiler</li>
                <li>Docker</li>
                <li>CI/CD</li>
                <li>VSCode / Emacs</li>
              </ul>
            </div>
          </div>

          <div>
            <SmallTitle>Academic</SmallTitle>
            Chenxi Li, Haoran Lin, Tian Tan, and Yue Li. (2025).
            {' '}
            <a
              href="https://doi.org/10.1145/3763081"
              target="_blank"
              rel="noopener noreferrer"
              className="break-words"
            >
              Two Approaches to Fast Bytecode Frontend for Static Analysis.
            </a>
            {' '}
            In <em className="italic">Proc. ACM Program. Lang. 9, OOPSLA2</em>,
            Article 303 (October 2025), 27 pages.
            <a
              href="https://doi.org/10.1145/3763081"
              target="_blank"
              rel="noopener noreferrer"
            >
              [DOI]
            </a>
          </div>
        </div>
      </div>
    </div>
  );
}

export default async function Index() {
  return (
    <main>
      <HomePage />
    </main>);
}
