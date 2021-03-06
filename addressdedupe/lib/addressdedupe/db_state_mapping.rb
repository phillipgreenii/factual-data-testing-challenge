module Addressdedupe
  #source https://www.usps.com/send/official-abbreviations.htm
  STATE_DB = {
    'AL' => 'AL',
    'ALABAMA' => 'AL',
    'AK' => 'AK',
    'ALASKA' => 'AK',
    'AS' => 'AS',
    'AMERICAN SAMOA' => 'AS',
    'AZ' => 'AZ',
    'ARIZONA' => 'AZ',
    'AR' => 'AR',
    'ARKANSAS' => 'AR',
    'CA' => 'CA',
    'CALIFORNIA' => 'CA',
    'CO' => 'CO',
    'COLORADO' => 'CO',
    'CT' => 'CT',
    'CONNECTICUT' => 'CT',
    'DE' => 'DE',
    'DELAWARE' => 'DE',
    'DC' => 'DC',
    'DISTRICT OF COLUMBIA' => 'DC',
    'FM' => 'FM',
    'FEDERATED STATES OF MICRONESIA' => 'FM',
    'FL' => 'FL',
    'FLORIDA' => 'FL',
    'GA' => 'GA',
    'GEORGIA' => 'GA',
    'GU' => 'GU',
    'GUAM GU' => 'GU',
    'HI' => 'HI',
    'HAWAII' => 'HI',
    'ID' => 'ID',
    'IDAHO' => 'ID',
    'IL' => 'IL',
    'ILLINOIS' => 'IL',
    'IN' => 'IN',
    'INDIANA' => 'IN',
    'IA' => 'IA',
    'IOWA' => 'IA',
    'KS' => 'KS',
    'KANSAS' => 'KS',
    'KY' => 'KY',
    'KENTUCKY' => 'KY',
    'LA' => 'LA',
    'LOUISIANA' => 'LA',
    'ME' => 'ME',
    'MAINE' => 'ME',
    'MH' => 'MH',
    'MARSHALL ISLANDS' => 'MH',
    'MD' => 'MD',
    'MARYLAND' => 'MD',
    'MA' => 'MA',
    'MASSACHUSETTS' => 'MA',
    'MI' => 'MI',
    'MICHIGAN' => 'MI',
    'MN' => 'MN',
    'MINNESOTA' => 'MN',
    'MS' => 'MS',
    'MISSISSIPPI' => 'MS',
    'MO' => 'MO',
    'MISSOURI' => 'MO',
    'MT' => 'MT',
    'MONTANA' => 'MT',
    'NE' => 'NE',
    'NEBRASKA' => 'NE',
    'NV' => 'NV',
    'NEVADA' => 'NV',
    'NH' => 'NH',
    'NEW HAMPSHIRE' => 'NH',
    'NJ' => 'NJ',
    'NEW JERSEY' => 'NJ',
    'NM' => 'NM',
    'NEW MEXICO' => 'NM',
    'NY' => 'NY',
    'NEW YORK' => 'NY',
    'NC' => 'NC',
    'NORTH CAROLINA' => 'NC',
    'ND' => 'ND',
    'NORTH DAKOTA' => 'ND',
    'MP' => 'MP',
    'NORTHERN MARIANA ISLANDS' => 'MP',
    'OH' => 'OH',
    'OHIO' => 'OH',
    'OK' => 'OK',
    'OKLAHOMA' => 'OK',
    'OR' => 'OR',
    'OREGON' => 'OR',
    'PW' => 'PW',
    'PALAU' => 'PW',
    'PA' => 'PA',
    'PENNSYLVANIA' => 'PA',
    'PR' => 'PR',
    'PUERTO RICO' => 'PR',
    'RI' => 'RI',
    'RHODE ISLAND' => 'RI',
    'SC' => 'SC',
    'SOUTH CAROLINA' => 'SC',
    'SD' => 'SD',
    'SOUTH DAKOTA' => 'SD',
    'TN' => 'TN',
    'TENNESSEE' => 'TN',
    'TX' => 'TX',
    'TEXAS' => 'TX',
    'UT' => 'UT',
    'UTAH' => 'UT',
    'VT' => 'VT',
    'VERMONT' => 'VT',
    'VI' => 'VI',
    'VIRGIN ISLANDS' => 'VI',
    'VA' => 'VA',
    'VIRGINIA' => 'VA',
    'WA' => 'WA',
    'WASHINGTON' => 'WA',
    'WV' => 'WV',
    'WEST VIRGINIA' => 'WV',
    'WI' => 'WI',
    'WISCONSIN' => 'WI',
    'WY' => 'WY',
    'WYOMING' => 'WY',
    'AE' => 'AE',
    'Armed Forces Africa' => 'AE',
    'AA' => 'AA',
    'Armed Forces Americas (except Canada)' => 'AA',
    'AE' => 'AE',
    'Armed Forces Canada' => 'AE',
    'AE' => 'AE',
    'Armed Forces Europe' => 'AE',
    'AE' => 'AE',
    'Armed Forces Middle East' => 'AE',
    'AP' => 'AP',
    'Armed Forces Pacific' => 'AP',
  }
end
