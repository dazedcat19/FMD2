{ Simple Translator

  Copyright (C) 2015 Nur Cholif

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit SimpleTranslator;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, strutils, gettext, LazFileUtils, LCLTranslator,
  Translations, LResources, Forms, LCLVersion;

type
  TLanguageItem = record
    id: String;
    name: String;
  end;

  TLanguageCollection = array of TLanguageItem;

const
  {
    https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
    //table[2]/tbody/tr[position()>1]/concat("('",td[5],"', '",td[3],"', '",td[4],"'),")
  }
  lang_codes: array[0..183] of array[0..2] of string = (
    ('ab', 'Abkhaz', 'аҧсуа бызшәа, аҧсшәа'),
    ('aa', 'Afar', 'Afaraf'),
    ('af', 'Afrikaans', 'Afrikaans'),
    ('ak', 'Akan', 'Akan'),
    ('sq', 'Albanian', 'Shqip'),
    ('am', 'Amharic', 'አማርኛ'),
    ('ar', 'Arabic', 'العربية'),
    ('an', 'Aragonese', 'aragonés'),
    ('hy', 'Armenian', 'Հայերեն'),
    ('as', 'Assamese', 'অসমীয়া'),
    ('av', 'Avaric', 'авар мацӀ, магӀарул мацӀ'),
    ('ae', 'Avestan', 'avesta'),
    ('ay', 'Aymara', 'aymar aru'),
    ('az', 'Azerbaijani', 'azərbaycan dili'),
    ('bm', 'Bambara', 'bamanankan'),
    ('ba', 'Bashkir', 'башҡорт теле'),
    ('eu', 'Basque', 'euskara, euskera'),
    ('be', 'Belarusian', 'беларуская мова'),
    ('bn', 'Bengali, Bangla', 'বাংলা'),
    ('bh', 'Bihari', 'भोजपुरी'),
    ('bi', 'Bislama', 'Bislama'),
    ('bs', 'Bosnian', 'bosanski jezik'),
    ('br', 'Breton', 'brezhoneg'),
    ('bg', 'Bulgarian', 'български език'),
    ('my', 'Burmese', 'ဗမာစာ'),
    ('ca', 'Catalan', 'català'),
    ('ch', 'Chamorro', 'Chamoru'),
    ('ce', 'Chechen', 'нохчийн мотт'),
    ('ny', 'Chichewa, Chewa, Nyanja', 'chiCheŵa, chinyanja'),
    ('zh', 'Chinese', '中文 (Zhōngwén), 汉语, 漢語'),
    ('cv', 'Chuvash', 'чӑваш чӗлхи'),
    ('kw', 'Cornish', 'Kernewek'),
    ('co', 'Corsican', 'corsu, lingua corsa'),
    ('cr', 'Cree', 'ᓀᐦᐃᔭᐍᐏᐣ'),
    ('hr', 'Croatian', 'hrvatski jezik'),
    ('cs', 'Czech', 'čeština, český jazyk'),
    ('da', 'Danish', 'dansk'),
    ('dv', 'Divehi, Dhivehi, Maldivian', 'ދިވެހި'),
    ('nl', 'Dutch', 'Nederlands, Vlaams'),
    ('dz', 'Dzongkha', 'རྫོང་ཁ'),
    ('en', 'English', 'English'),
    ('eo', 'Esperanto', 'Esperanto'),
    ('et', 'Estonian', 'eesti, eesti keel'),
    ('ee', 'Ewe', 'Eʋegbe'),
    ('fo', 'Faroese', 'føroyskt'),
    ('fj', 'Fijian', 'vosa Vakaviti'),
    ('fi', 'Finnish', 'suomi, suomen kieli'),
    ('fr', 'French', 'français, langue française'),
    ('ff', 'Fula, Fulah, Pulaar, Pular', 'Fulfulde, Pulaar, Pular'),
    ('gl', 'Galician', 'galego'),
    ('ka', 'Georgian', 'ქართული'),
    ('de', 'German', 'Deutsch'),
    ('el', 'Greek (modern)', 'ελληνικά'),
    ('gn', 'Guaraní', 'Avañe''ẽ'),
    ('gu', 'Gujarati', 'ગુજરાતી'),
    ('ht', 'Haitian, Haitian Creole', 'Kreyòl ayisyen'),
    ('ha', 'Hausa', '(Hausa) هَوُسَ'),
    ('he', 'Hebrew (modern)', 'עברית'),
    ('hz', 'Herero', 'Otjiherero'),
    ('hi', 'Hindi', 'हिन्दी, हिंदी'),
    ('ho', 'Hiri Motu', 'Hiri Motu'),
    ('hu', 'Hungarian', 'magyar'),
    ('ia', 'Interlingua', 'Interlingua'),
    ('id', 'Indonesian', 'Bahasa Indonesia'),
    ('ie', 'Interlingue', 'Originally called Occidental; then Interlingue after WWII'),
    ('ga', 'Irish', 'Gaeilge'),
    ('ig', 'Igbo', 'Asụsụ Igbo'),
    ('ik', 'Inupiaq', 'Iñupiaq, Iñupiatun'),
    ('io', 'Ido', 'Ido'),
    ('is', 'Icelandic', 'Íslenska'),
    ('it', 'Italian', 'Italiano'),
    ('iu', 'Inuktitut', 'ᐃᓄᒃᑎᑐᑦ'),
    ('ja', 'Japanese', '日本語 (にほんご)'),
    ('jv', 'Javanese', 'ꦧꦱꦗꦮ, Basa Jawa'),
    ('kl', 'Kalaallisut, Greenlandic', 'kalaallisut, kalaallit oqaasii'),
    ('kn', 'Kannada', 'ಕನ್ನಡ'),
    ('kr', 'Kanuri', 'Kanuri'),
    ('ks', 'Kashmiri', 'कश्मीरी, كشميري‎'),
    ('kk', 'Kazakh', 'қазақ тілі'),
    ('km', 'Khmer', 'ខ្មែរ, ខេមរភាសា, ភាសាខ្មែរ'),
    ('ki', 'Kikuyu, Gikuyu', 'Gĩkũyũ'),
    ('rw', 'Kinyarwanda', 'Ikinyarwanda'),
    ('ky', 'Kyrgyz', 'Кыргызча, Кыргыз тили'),
    ('kv', 'Komi', 'коми кыв'),
    ('kg', 'Kongo', 'Kikongo'),
    ('ko', 'Korean', '한국어'),
    ('ku', 'Kurdish', 'Kurdî, كوردی‎'),
    ('kj', 'Kwanyama, Kuanyama', 'Kuanyama'),
    ('la', 'Latin', 'latine, lingua latina'),
    ('lb', 'Luxembourgish, Letzeburgesch', 'Lëtzebuergesch'),
    ('lg', 'Ganda', 'Luganda'),
    ('li', 'Limburgish, Limburgan, Limburger', 'Limburgs'),
    ('ln', 'Lingala', 'Lingála'),
    ('lo', 'Lao', 'ພາສາລາວ'),
    ('lt', 'Lithuanian', 'lietuvių kalba'),
    ('lu', 'Luba-Katanga', 'Tshiluba'),
    ('lv', 'Latvian', 'latviešu valoda'),
    ('gv', 'Manx', 'Gaelg, Gailck'),
    ('mk', 'Macedonian', 'македонски јазик'),
    ('mg', 'Malagasy', 'fiteny malagasy'),
    ('ms', 'Malay', 'bahasa Melayu, بهاس ملايو‎'),
    ('ml', 'Malayalam', 'മലയാളം'),
    ('mt', 'Maltese', 'Malti'),
    ('mi', 'Māori', 'te reo Māori'),
    ('mr', 'Marathi (Marāṭhī)', 'मराठी'),
    ('mh', 'Marshallese', 'Kajin M̧ajeļ'),
    ('mn', 'Mongolian', 'Монгол хэл'),
    ('na', 'Nauruan', 'Dorerin Naoero'),
    ('nv', 'Navajo, Navaho', 'Diné bizaad'),
    ('nd', 'Northern Ndebele', 'isiNdebele'),
    ('ne', 'Nepali', 'नेपाली'),
    ('ng', 'Ndonga', 'Owambo'),
    ('nb', 'Norwegian Bokmål', 'Norsk bokmål'),
    ('nn', 'Norwegian Nynorsk', 'Norsk nynorsk'),
    ('no', 'Norwegian', 'Norsk'),
    ('ii', 'Nuosu', 'ꆈꌠ꒿ Nuosuhxop'),
    ('nr', 'Southern Ndebele', 'isiNdebele'),
    ('oc', 'Occitan', 'occitan, lenga d''òc'),
    ('oj', 'Ojibwe, Ojibwa', 'ᐊᓂᔑᓈᐯᒧᐎᓐ'),
    ('cu', 'Old Church Slavonic, Church Slavonic, Old Bulgarian', 'ѩзыкъ словѣньскъ'),
    ('om', 'Oromo', 'Afaan Oromoo'),
    ('or', 'Oriya', 'ଓଡ଼ିଆ'),
    ('os', 'Ossetian, Ossetic', 'ирон æвзаг'),
    ('pa', 'Eastern Punjabi, Eastern Panjabi', 'ਪੰਜਾਬੀ'),
    ('pi', 'Pāli', 'पाऴि'),
    ('fa', 'Persian (Farsi)', 'فارسی'),
    ('pl', 'Polish', 'język polski, polszczyzna'),
    ('ps', 'Pashto, Pushto', 'پښتو'),
    ('pt', 'Portuguese', 'Português'),
    ('qu', 'Quechua', 'Runa Simi, Kichwa'),
    ('rm', 'Romansh', 'rumantsch grischun'),
    ('rn', 'Kirundi', 'Ikirundi'),
    ('ro', 'Romanian', 'Română'),
    ('ru', 'Russian', 'Русский'),
    ('sa', 'Sanskrit (Saṁskṛta)', 'संस्कृतम्'),
    ('sc', 'Sardinian', 'sardu'),
    ('sd', 'Sindhi', 'सिन्धी, سنڌي، سندھی‎'),
    ('se', 'Northern Sami', 'Davvisámegiella'),
    ('sm', 'Samoan', 'gagana fa''a Samoa'),
    ('sg', 'Sango', 'yângâ tî sängö'),
    ('sr', 'Serbian', 'српски језик'),
    ('gd', 'Scottish Gaelic, Gaelic', 'Gàidhlig'),
    ('sn', 'Shona', 'chiShona'),
    ('si', 'Sinhalese, Sinhala', 'සිංහල'),
    ('sk', 'Slovak', 'slovenčina, slovenský jazyk'),
    ('sl', 'Slovene', 'slovenski jezik, slovenščina'),
    ('so', 'Somali', 'Soomaaliga, af Soomaali'),
    ('st', 'Southern Sotho', 'Sesotho'),
    ('es', 'Spanish', 'Español'),
    ('su', 'Sundanese', 'Basa Sunda'),
    ('sw', 'Swahili', 'Kiswahili'),
    ('ss', 'Swati', 'SiSwati'),
    ('sv', 'Swedish', 'svenska'),
    ('ta', 'Tamil', 'தமிழ்'),
    ('te', 'Telugu', 'తెలుగు'),
    ('tg', 'Tajik', 'тоҷикӣ, toçikī, تاجیکی‎'),
    ('th', 'Thai', 'ไทย'),
    ('ti', 'Tigrinya', 'ትግርኛ'),
    ('bo', 'Tibetan Standard, Tibetan, Central', 'བོད་ཡིག'),
    ('tk', 'Turkmen', 'Türkmen, Түркмен'),
    ('tl', 'Tagalog', 'Wikang Tagalog'),
    ('tn', 'Tswana', 'Setswana'),
    ('to', 'Tonga (Tonga Islands)', 'faka Tonga'),
    ('tr', 'Turkish', 'Türkçe'),
    ('ts', 'Tsonga', 'Xitsonga'),
    ('tt', 'Tatar', 'татар теле, tatar tele'),
    ('tw', 'Twi', 'Twi'),
    ('ty', 'Tahitian', 'Reo Tahiti'),
    ('ug', 'Uyghur', 'ئۇيغۇرچە‎, Uyghurche'),
    ('uk', 'Ukrainian', 'Українська'),
    ('ur', 'Urdu', 'اردو'),
    ('uz', 'Uzbek', 'Oʻzbek, Ўзбек, أۇزبېك‎'),
    ('ve', 'Venda', 'Tshivenḓa'),
    ('vi', 'Vietnamese', 'Tiếng Việt'),
    ('vo', 'Volapük', 'Volapük'),
    ('wa', 'Walloon', 'walon'),
    ('cy', 'Welsh', 'Cymraeg'),
    ('wo', 'Wolof', 'Wollof'),
    ('fy', 'Western Frisian', 'Frysk'),
    ('xh', 'Xhosa', 'isiXhosa'),
    ('yi', 'Yiddish', 'ייִדיש'),
    ('yo', 'Yoruba', 'Yorùbá'),
    ('za', 'Zhuang, Chuang', 'Saɯ cueŋƅ, Saw cuengh'),
    ('zu', 'Zulu', 'isiZulu')
  );

  {
    https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2
    //table[3]/tbody/tr[position()>1]/concat("('",td[1],"', '",td[2],"'),")
  }
  country_codes: array[0..248] of array[0..1] of string = (
    ('AD', 'Andorra'),
    ('AE', 'United Arab Emirates'),
    ('AF', 'Afghanistan'),
    ('AG', 'Antigua and Barbuda'),
    ('AI', 'Anguilla'),
    ('AL', 'Albania'),
    ('AM', 'Armenia'),
    ('AO', 'Angola'),
    ('AQ', 'Antarctica'),
    ('AR', 'Argentina'),
    ('AS', 'American Samoa'),
    ('AT', 'Austria'),
    ('AU', 'Australia'),
    ('AW', 'Aruba'),
    ('AX', 'Aland Islands !Åland Islands'),
    ('AZ', 'Azerbaijan'),
    ('BA', 'Bosnia and Herzegovina'),
    ('BB', 'Barbados'),
    ('BD', 'Bangladesh'),
    ('BE', 'Belgium'),
    ('BF', 'Burkina Faso'),
    ('BG', 'Bulgaria'),
    ('BH', 'Bahrain'),
    ('BI', 'Burundi'),
    ('BJ', 'Benin'),
    ('BL', 'Saint Barthélemy'),
    ('BM', 'Bermuda'),
    ('BN', 'Brunei Darussalam'),
    ('BO', 'Bolivia, Plurinational State of'),
    ('BQ', 'Bonaire, Sint Eustatius and Saba'),
    ('BR', 'Brazil'),
    ('BS', 'Bahamas'),
    ('BT', 'Bhutan'),
    ('BV', 'Bouvet Island'),
    ('BW', 'Botswana'),
    ('BY', 'Belarus'),
    ('BZ', 'Belize'),
    ('CA', 'Canada'),
    ('CC', 'Cocos (Keeling) Islands'),
    ('CD', 'Congo, the Democratic Republic of the'),
    ('CF', 'Central African Republic'),
    ('CG', 'Congo'),
    ('CH', 'Switzerland'),
    ('CI', 'Cote d''Ivoire !Côte d''Ivoire'),
    ('CK', 'Cook Islands'),
    ('CL', 'Chile'),
    ('CM', 'Cameroon'),
    ('CN', 'China'),
    ('CO', 'Colombia'),
    ('CR', 'Costa Rica'),
    ('CU', 'Cuba'),
    ('CV', 'Cabo Verde'),
    ('CW', 'Curaçao'),
    ('CX', 'Christmas Island'),
    ('CY', 'Cyprus'),
    ('CZ', 'Czechia'),
    ('DE', 'Germany'),
    ('DJ', 'Djibouti'),
    ('DK', 'Denmark'),
    ('DM', 'Dominica'),
    ('DO', 'Dominican Republic'),
    ('DZ', 'Algeria'),
    ('EC', 'Ecuador'),
    ('EE', 'Estonia'),
    ('EG', 'Egypt'),
    ('EH', 'Western Sahara'),
    ('ER', 'Eritrea'),
    ('ES', 'Spain'),
    ('ET', 'Ethiopia'),
    ('FI', 'Finland'),
    ('FJ', 'Fiji'),
    ('FK', 'Falkland Islands (Malvinas)'),
    ('FM', 'Micronesia, Federated States of'),
    ('FO', 'Faroe Islands'),
    ('FR', 'France'),
    ('GA', 'Gabon'),
    ('GB', 'United Kingdom of Great Britain and Northern Ireland'),
    ('GD', 'Grenada'),
    ('GE', 'Georgia'),
    ('GF', 'French Guiana'),
    ('GG', 'Guernsey'),
    ('GH', 'Ghana'),
    ('GI', 'Gibraltar'),
    ('GL', 'Greenland'),
    ('GM', 'Gambia'),
    ('GN', 'Guinea'),
    ('GP', 'Guadeloupe'),
    ('GQ', 'Equatorial Guinea'),
    ('GR', 'Greece'),
    ('GS', 'South Georgia and the South Sandwich Islands'),
    ('GT', 'Guatemala'),
    ('GU', 'Guam'),
    ('GW', 'Guinea-Bissau'),
    ('GY', 'Guyana'),
    ('HK', 'Hong Kong'),
    ('HM', 'Heard Island and McDonald Islands'),
    ('HN', 'Honduras'),
    ('HR', 'Croatia'),
    ('HT', 'Haiti'),
    ('HU', 'Hungary'),
    ('ID', 'Indonesia'),
    ('IE', 'Ireland'),
    ('IL', 'Israel'),
    ('IM', 'Isle of Man'),
    ('IN', 'India'),
    ('IO', 'British Indian Ocean Territory'),
    ('IQ', 'Iraq'),
    ('IR', 'Iran, Islamic Republic of'),
    ('IS', 'Iceland'),
    ('IT', 'Italy'),
    ('JE', 'Jersey'),
    ('JM', 'Jamaica'),
    ('JO', 'Jordan'),
    ('JP', 'Japan'),
    ('KE', 'Kenya'),
    ('KG', 'Kyrgyzstan'),
    ('KH', 'Cambodia'),
    ('KI', 'Kiribati'),
    ('KM', 'Comoros'),
    ('KN', 'Saint Kitts and Nevis'),
    ('KP', 'Korea, Democratic People''s Republic of'),
    ('KR', 'Korea, Republic of'),
    ('KW', 'Kuwait'),
    ('KY', 'Cayman Islands'),
    ('KZ', 'Kazakhstan'),
    ('LA', 'Lao People''s Democratic Republic'),
    ('LB', 'Lebanon'),
    ('LC', 'Saint Lucia'),
    ('LI', 'Liechtenstein'),
    ('LK', 'Sri Lanka'),
    ('LR', 'Liberia'),
    ('LS', 'Lesotho'),
    ('LT', 'Lithuania'),
    ('LU', 'Luxembourg'),
    ('LV', 'Latvia'),
    ('LY', 'Libya'),
    ('MA', 'Morocco'),
    ('MC', 'Monaco'),
    ('MD', 'Moldova, Republic of'),
    ('ME', 'Montenegro'),
    ('MF', 'Saint Martin (French part)'),
    ('MG', 'Madagascar'),
    ('MH', 'Marshall Islands'),
    ('MK', 'Macedonia, the former Yugoslav Republic of'),
    ('ML', 'Mali'),
    ('MM', 'Myanmar'),
    ('MN', 'Mongolia'),
    ('MO', 'Macao'),
    ('MP', 'Northern Mariana Islands'),
    ('MQ', 'Martinique'),
    ('MR', 'Mauritania'),
    ('MS', 'Montserrat'),
    ('MT', 'Malta'),
    ('MU', 'Mauritius'),
    ('MV', 'Maldives'),
    ('MW', 'Malawi'),
    ('MX', 'Mexico'),
    ('MY', 'Malaysia'),
    ('MZ', 'Mozambique'),
    ('NA', 'Namibia'),
    ('NC', 'New Caledonia'),
    ('NE', 'Niger'),
    ('NF', 'Norfolk Island'),
    ('NG', 'Nigeria'),
    ('NI', 'Nicaragua'),
    ('NL', 'Netherlands'),
    ('NO', 'Norway'),
    ('NP', 'Nepal'),
    ('NR', 'Nauru'),
    ('NU', 'Niue'),
    ('NZ', 'New Zealand'),
    ('OM', 'Oman'),
    ('PA', 'Panama'),
    ('PE', 'Peru'),
    ('PF', 'French Polynesia'),
    ('PG', 'Papua New Guinea'),
    ('PH', 'Philippines'),
    ('PK', 'Pakistan'),
    ('PL', 'Poland'),
    ('PM', 'Saint Pierre and Miquelon'),
    ('PN', 'Pitcairn'),
    ('PR', 'Puerto Rico'),
    ('PS', 'Palestine, State of'),
    ('PT', 'Portugal'),
    ('PW', 'Palau'),
    ('PY', 'Paraguay'),
    ('QA', 'Qatar'),
    ('RE', 'Reunion !Réunion'),
    ('RO', 'Romania'),
    ('RS', 'Serbia'),
    ('RU', 'Russian Federation'),
    ('RW', 'Rwanda'),
    ('SA', 'Saudi Arabia'),
    ('SB', 'Solomon Islands'),
    ('SC', 'Seychelles'),
    ('SD', 'Sudan'),
    ('SE', 'Sweden'),
    ('SG', 'Singapore'),
    ('SH', 'Saint Helena, Ascension and Tristan da Cunha'),
    ('SI', 'Slovenia'),
    ('SJ', 'Svalbard and Jan Mayen'),
    ('SK', 'Slovakia'),
    ('SL', 'Sierra Leone'),
    ('SM', 'San Marino'),
    ('SN', 'Senegal'),
    ('SO', 'Somalia'),
    ('SR', 'Suriname'),
    ('SS', 'South Sudan'),
    ('ST', 'Sao Tome and Principe'),
    ('SV', 'El Salvador'),
    ('SX', 'Sint Maarten (Dutch part)'),
    ('SY', 'Syrian Arab Republic'),
    ('SZ', 'Swaziland'),
    ('TC', 'Turks and Caicos Islands'),
    ('TD', 'Chad'),
    ('TF', 'French Southern Territories'),
    ('TG', 'Togo'),
    ('TH', 'Thailand'),
    ('TJ', 'Tajikistan'),
    ('TK', 'Tokelau'),
    ('TL', 'Timor-Leste'),
    ('TM', 'Turkmenistan'),
    ('TN', 'Tunisia'),
    ('TO', 'Tonga'),
    ('TR', 'Turkey'),
    ('TT', 'Trinidad and Tobago'),
    ('TV', 'Tuvalu'),
    ('TW', 'Taiwan, Province of China'),
    ('TZ', 'Tanzania, United Republic of'),
    ('UA', 'Ukraine'),
    ('UG', 'Uganda'),
    ('UM', 'United States Minor Outlying Islands'),
    ('US', 'United States of America'),
    ('UY', 'Uruguay'),
    ('UZ', 'Uzbekistan'),
    ('VA', 'Holy See'),
    ('VC', 'Saint Vincent and the Grenadines'),
    ('VE', 'Venezuela, Bolivarian Republic of'),
    ('VG', 'Virgin Islands, British'),
    ('VI', 'Virgin Islands, U.S.'),
    ('VN', 'Viet Nam'),
    ('VU', 'Vanuatu'),
    ('WF', 'Wallis and Futuna'),
    ('WS', 'Samoa'),
    ('YE', 'Yemen'),
    ('YT', 'Mayotte'),
    ('ZA', 'South Africa'),
    ('ZM', 'Zambia'),
    ('ZW', 'Zimbabwe')
  );

  ldir: array[0..3] of string =
    ('LANG', 'languages', 'locale', 'locale' + PathDelim + 'LC_MESSAGES');

var
  AvailableLanguages: TStringList;
  LastSelected: string = '';
  LangDir: string = '';
  LangAppName: string = '';

  procedure CollectLanguagesFiles(const appname: string = ''; const dir: string = '';
    useNativeName: Boolean = True);
  function GetLangName(const lcode: string; const useNativeName: Boolean = True): string;

  function SetLang(const lang: string; appname: string = ''): Boolean;
  function SetLangByIndex(const Index: Integer): Boolean;
  function GetDefaultLang: string; inline;

implementation

function SortValue(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareStr(List.ValueFromIndex[Index1], List.ValueFromIndex[Index2]);
end;

procedure CollectLanguagesFiles(const appname: string; const dir: string;
  useNativeName: Boolean);

  procedure searchLangDir(adir, aname: string);
  var
    SR: TSearchRec;
    p: Integer;
    ldir, lname, s, id: string;
  begin
    ldir := adir;
    lname := LowerCase(aname);
    if RightStr(ldir, 1) <> PathDelim then
      ldir := ldir + PathDelim;
    if DirectoryExists(ldir) then
    begin
      if FindFirst(ldir + '*', faAnyFile, SR) = 0 then
        repeat
          s := LowerCase(SR.Name);
          if (Pos(lname + '.', s) = 1) and
            ((RightStr(s, 3) = '.po') or (RightStr(s, 3) = '.mo')) then
          begin
            s := SR.Name;
            SetLength(s, Length(s) - 3);
            p := Pos('.', s);
            if p > 0 then
            begin
              id := Copy(s, p + 1, Length(s));
              if AvailableLanguages.Values[id] = '' then
                AvailableLanguages.Values[id] := GetLangName(id, useNativeName);
            end;
          end;
          until FindNext(SR) <> 0;
        FindClose(SR);
    end;
  end;

var
  sdir, tdir, tappname: string;
  lauto: Boolean = False;
  i: Integer;
begin
  tdir := dir;
  if tdir = '' then
  begin
    if LangDir <> '' then
      tdir := LangDir
    else
    begin
      tdir := ExtractFilePath(Application.ExeName);
      lauto := True;
    end;
  end;
  tappname := appname;
  if tappname = '' then
  begin
    if LangAppName <> '' then
      tappname := LangAppName
    else
      tappname := ExtractFileNameOnly(ParamStr(0));
  end;
  AvailableLanguages.Clear;

  if lauto then
  begin
    for i := Low(ldir) to High(ldir) do
    begin
      sdir := tdir + ldir[i] + PathDelim;
      searchLangDir(sdir, tappname);
    end;
  end
  else
  begin
    searchLangDir(tdir, tappname);
  end;

  if AvailableLanguages.Count > 0 then
    AvailableLanguages.CustomSort(@SortValue);
end;

function GetLangName(const lcode: string; const useNativeName: Boolean): string;

  function getlangcodes(const l: string): string;
  var
    i: Integer;
  begin
    Result := l;
    for i := Low(lang_codes) to High(lang_codes) do
      if SameText(l, lang_codes[i, 0]) then
      begin
        if useNativeName then
          Result := lang_codes[i, 2]
        else
          Result := lang_codes[i, 1];
        Break;
      end;
  end;

  function getcountrycodes(l: string): string;
  var
    i: Integer;
  begin
    Result := l;
    for i := Low(country_codes) to High(country_codes) do
    begin
      if SameText(l, country_codes[i, 0]) then
      begin
        Result := country_codes[i, 1];
        Break;
      end;
    end;
  end;

var
  p: Integer;
  s, id: String;
begin
  Result := lcode;
  if Result = '' then Exit;
  s := TrimSet(lcode,[' ','.','_','-']);
  p := Pos('_', s);
  if p = 0 then
    p := Pos('-', s);
  if p > 1 then
  begin
    id := Copy(s, 1, p - 1);
    Result := getlangcodes(id);
    id := Copy(s, P + 1, Length(s));
    s := getcountrycodes(id);
    if s <> id then
      Result := Format('%s (%s)', [Result, s]);
  end
  else
    Result := getlangcodes(s);
end;

function TranslateLCL(Lang: string): Boolean;
var
  lcllang, lcllangdir, lcllangpath: string;
  mofile: Boolean;

  procedure FindLCLFile;
  var
    i: Integer;
    l, s: string;
  begin
    if LangDir <> '' then
    begin
      lcllangdir := LangDir;
      if RightStr(lcllangdir, 1) <> PathDelim then
        lcllangdir := lcllangdir + PathDelim;
      s := lcllangdir + 'lclstrconsts.' + lcllang;
      if FileExists(s + '.po') then
        lcllangpath := s + '.po'
      else if FileExists(s + '.mo') then
      begin
        lcllangpath := s + '.mo';
        mofile := True;
      end;
    end;
    if lcllangpath = '' then
    begin
      l := ExtractFilePath(Application.ExeName);
      for i := Low(ldir) to High(ldir) do
      begin
        lcllangdir := l + ldir[i];
        s := lcllangdir + 'lclstrconsts.' + lcllang;
        if FileExists(s + '.po') then
        begin
          lcllangpath := s + '.po';
          Break;
        end
        else if FileExists(s + '.mo') then
        begin
          lcllangpath := s + '.mo';
          mofile := True;
          Break;
        end;
      end;
    end;
  end;

begin
  Result := False;
  lcllangpath := '';
  mofile := False;
  lcllang := Lang;
  FindLCLFile;
  if lcllangpath = '' then
  begin
    if Pos('_', lcllang) <> 0 then
      SetLength(lcllang, Pos('_', lcllang)-1);
    FindLCLFile;
  end;
  if lcllangpath <> '' then
  begin
    if mofile then
      gettext.TranslateUnitResourceStrings('LclStrConsts', lcllangpath)
    else
      Translations.TranslateUnitResourceStrings('LclStrConsts', lcllangpath);
    Result := True;
  end;
end;

function SetLang(const lang: string; appname: string): Boolean;
var
  lfile: string;
  ltrans: TUpdateTranslator;
  i: Integer;
begin
  Result := False;
  ltrans := nil;
  if (LastSelected <> lang) then
  begin
    LangDir := TrimRightSet(LangDir, [PathDelim]);
    if appname = '' then
    begin
      if LangAppName <> '' then
        appname := LangAppName
      else
        appname := ExtractFileNameOnly(ParamStr(0));
    end;
    lfile := LangDir + PathDelim + appname + '.' + lang;

    if FileExists(lfile + '.po') then //po file
    begin
      lfile := lfile + '.po';
      Translations.TranslateResourceStrings(lfile);
      ltrans := TPOTranslator.Create(lfile);
    end
    else
    if FileExists(lfile + '.mo') then //mo file
    begin
      lfile := lfile + '.mo';
      gettext.TranslateResourceStrings(lfile);
      ltrans := TDefaultTranslator.Create(lfile);
    end;

    TranslateLCL(lang);

    if ltrans <> nil then
    begin
      if Assigned(LRSTranslator) then
        LRSTranslator.Free;
      LRSTranslator := ltrans;
      for i := 0 to Screen.CustomFormCount-1 do
        ltrans.UpdateTranslation(Screen.CustomForms[i]);
      for i := 0 to Screen.DataModuleCount-1 do
        ltrans.UpdateTranslation(Screen.DataModules[i]);
      LastSelected := lang;
      Result := True;
    end;
  end;
end;

function SetLangByIndex(const Index: Integer): Boolean;
begin
  Result := False;
  if Index < 0 then Exit;
  if LastSelected <> AvailableLanguages.Names[Index] then
    Result := SetLang(AvailableLanguages.Names[Index]);
end;

function GetDefaultLang: string;
begin
  {$if lcl_fullversion > 2000600}
  Result := LCLTranslator.SetDefaultLang('');
  {$else}
  Result := LCLTranslator.GetDefaultLang;
  {$ifend}
end;

initialization
  AvailableLanguages := TStringList.Create;
  AvailableLanguages.NameValueSeparator := '=';

finalization
  AvailableLanguages.Free;

end.

