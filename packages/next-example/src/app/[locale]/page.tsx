import { useTranslations } from 'next-intl';
import { unstable_setRequestLocale } from 'next-intl/server';

function HomePage() {
  const t = useTranslations('Index');

  return (
    <div className='flex h-[60vh]'>
      <div className='m-auto'>
        <div>
          <div className='text-4xl text-bold'>{t("title")}</div>
        </div>

        <div className='mt-10'>
          <h2 className='text-2xl text-bold'>
            {t("contract_title")}
          </h2>

          <ul className='mt-3'>
            <li>{t("contract")}</li>
          </ul>
        </div>
      </div>
    </div>
  );
}

export default function Index({ params }: {
  params: { locale: string }
}) {
  unstable_setRequestLocale(params.locale);

  return (
    <main>
      <HomePage />
    </main>);
}