# mileage-counter
Keep track of flight mileage earned on business trips.

## Purpose
A Japanese company has a rule that all flight mileage earned on business trips paid by the company must not be redeemed for private trips.
The situation gets complicated when an employee also earn mileage by other methods such as shopping with associated credit cards.
Using this programme,
the employee can separately record the mileage accrued on business trips and whaever excess mileage he/she has is all his/hers.

Indeed, the code is so small that the data is also included in the code. A line starting from three '-' signs (i.e. `---`) show a record entry, which consists of

1. Date
2. Mileage
3. Bonus mileage
4. Memo

If certain amount of mileage was redeemed for a business trip, that mileage can be recorded by negative mileage. When redeeming, only the base mileage are used and the bonus mileage is left intact. I do not know if this is the right way to do.

A beauty of this code is to keep track of expired mileage. The rule typical in Japanese airlines is followed where mileage expires at the end of the 36th month.

## Output

	% ./WorkMileageCounter.hs
	[   1] 16/05/2009                  5882 (   588) Narita - Papeete (expiry 31/05/2012)
	                   5882 (   588)
	[   4] 26/06/2009                  2432 (     0) Sydney - Narita (expiry 30/06/2012)
	[   3] 26/06/2009                   319 (     0) Melbourne - Sydney (expiry 30/06/2012)
	[   2] 26/06/2009                   262 (     0) Hobart - Melbourne (expiry 30/06/2012)
	                   8895 (   588)
	[   5] 06/07/2009                   510 (    51) Haneda - Chitose (expiry 31/07/2012)
	                   9405 (   639)
	[   6] 08/07/2009                   510 (    51) Chitose - Haneda (expiry 31/07/2012)
	                   9915 (   690)
	[   7] 19/06/2010                  3404 (   736) Narita - Sydney (expiry 30/06/2013)
	                  13319 (  1426)
	[   8] 25/06/2010                  3404 (   736) Sydney - Narita (expiry 30/06/2013)
	                  16723 (  2162)
	REDEEM 06/08/2010                -10000
	                   6723 (  2162)
	[  10] 20/08/2010                   425 (    57) Haneda - Fukuoka (expiry 31/08/2013)
	                   7148 (  2219)
	[  11] 22/08/2010                   567 (    57) Fukuoka - Haneda (expiry 31/08/2013)
	                   7715 (  2276)
	EXPIRE 31/05/2012                     0 (  -588) flight [1] (16/05/2009)
	                   7715 (  1688)
	                   7715 (  1688)
	EXPIRE 31/07/2012                     0 (   -51) flight [6] (08/07/2009)
	EXPIRE 31/07/2012                     0 (   -51) flight [5] (06/07/2009)
	                   7715 (  1586)
	EXPIRE 30/06/2013                 -3319 (  -736) flight [7] (19/06/2010)
	EXPIRE 30/06/2013                 -3404 (  -736) flight [8] (25/06/2010)
	                    992 (   114)
	EXPIRE 31/08/2013                  -567 (   -57) flight [11] (22/08/2010)
	EXPIRE 31/08/2013                  -425 (   -57) flight [10] (20/08/2010)

In December 2010, for example, your work mile was 7715 base miles
plus 2276 bonus miles. On 31 May 2012, the bonus miles of 588 miles which had been
earned on the 16 May 2009 flight expired but the base miles earned on the same flight
had been redeemed on 6 Aug 2010 such that they did not expire.
