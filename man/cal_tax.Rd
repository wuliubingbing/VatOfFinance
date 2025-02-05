% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vat.R
\name{cal_tax}
\alias{cal_tax}
\title{Calculate VAT}
\usage{
cal_tax(buy_price, sell_price, tax_rate = 0.06)
}
\arguments{
\item{buy_price}{The price of the product when it was bought}

\item{sell_price}{The price of the product when it was sold}

\item{tax_rate}{The tax rate}
}
\value{
The VAT of the transaction
}
\description{
This function calculates the VAT of a transaction
}
\examples{
cal_tax(100, 110, 0.06)
}
