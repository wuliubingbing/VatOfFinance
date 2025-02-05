#' @title Calculate VAT
#' @description This function calculates the VAT of a transaction
#' @param buy_price The price of the product when it was bought
#' @param sell_price The price of the product when it was sold
#' @param tax_rate The tax rate
#' @return The VAT of the transaction
#' @examples
#' cal_tax(100, 110, 0.06)
#' @export
cal_tax <- function(buy_price,sell_price, tax_rate=0.06){
  tax <- (sell_price - buy_price)/(1+tax_rate)*tax_rate
  return(tax)
}

#' @title Calculate transaction records VAT
#' @description This function calculates the VAT of a series of transaction records.Moving weighted average method is used to calculate the average holding price.
#' @import dplyr
#' @param x A data frame containing the transaction records
#' @param amount The amount column name of the transaction
#' @param volume The volume column name of the transaction
#' @param tax_rate The tax rate
#' @param ... The director column name of the transaction,can be null or length 1
#' @return A data frame containing the transaction records with VAT calculated
#' @examples
#' data(records)
#' records |> dplyr::filter(symbol=='a') |> cal_transaction_records()
#' @export
cal_transaction_records <- function(x,amount='amount',volume='volume',tax_rate=0.06,...){
  wlbb=list(...)
  if(length(wlbb)==1){
    director=wlbb[[1]]
    x=x %>% dplyr::mutate(TransactionPrice=abs(.data[[amount]]/.data[[volume]]),
                    EndingVolume=cumsum(.data[[volume]]*.data[[director]]),
                    AverageHoldPrice=abs(.data[[amount]]/.data[[volume]]))
    if(nrow(x)>1){
      for(i in 2:nrow(x)){
        x$AverageHoldPrice[i]=if_else(x[[director]][i]>0,
                                      (x$EndingVolume[i-1]*x$AverageHoldPrice[i-1]+x$volume[i]*x$TransactionPrice[i])/x$EndingVolume[i],
                                      x$AverageHoldPrice[i-1])
      }
    }
    diff1 <- function(x){c(x[1],diff(x,lag=1))}
    x=x %>% dplyr::mutate(EndingTotalCost=EndingVolume*AverageHoldPrice,
                          CostChange=diff1(EndingTotalCost),
                          TaxIncludeProfit=if_else(.data[[director]]>0,0,.data[[volume]]*(TransactionPrice-dplyr::lag(AverageHoldPrice,n=1L,default=0))),
                          VAT=(TaxIncludeProfit/(1+tax_rate))*tax_rate)
  }else if(length(wlbb)==0){
    x=x %>% dplyr::mutate(director=-sign(.data[[amount]])) %>%
      cal_transaction_records(amount=amount,volume=volume,tax_rate=tax_rate,director='director')
  }else{
    stop("The length of the argument ... should be 0 or 1")
  }

  if(any(x$EndingVolume<0)){message("Warning:One line EndingVolume < 0")}
  if(any(x$AverageHoldPrice<0)){message("Warning:One line AverageHoldPrice < 0")}
  return(x)
}

#' @title Calculate multiply transaction records VAT
#' @description This function calculates the VAT of a series of transaction records.Moving weighted average method is used to calculate the average holding price.
#' @import dplyr
#' @importFrom plyr ddply
#' @importFrom plyr .
#' @import glue
#' @param x A data frame containing the multiply transaction records
#' @param symbol The symbol column name of the transaction
#' @param amount The amount column name of the transaction
#' @param volume The volume column name of the transaction
#' @param tax_rate The tax rate
#' @param ... The director column name of the transaction,can be null or length 1
#' @return A data frame containing the transaction records with VAT calculated
#' @examples
#' data(records)
#' cal_multiply_records(x=records)
#' @export
cal_multiply_records <- function(x,symbol='symbol',amount='amount',volume='volume',tax_rate=0.06,...){
  x <- x %>% plyr::ddply(.(symbol),cal_transaction_records,amount=amount,volume=volume,tax_rate=tax_rate,...)
  return(x)
}

cal_financial_VAT <- function(date,vat,tax_rate=0.06){
  vat <- vat %>% dplyr::filter(date==date)
  return(sum(vat$VAT))
}


#' @title Calculate financial transfer VAT
#' @description This function calculates the VAT of financial transfer.See also https://shanghai.chinatax.gov.cn/tax/zcfw/rdwd/202402/t470599.html
#' @param dates A vector of dates
#' @param differences A vector of differences
#' @param vat_rate The tax rate
#' @return A data frame containing the VAT of financial transfer
#' @examples
#' dates <- as.Date(c("2021-01-01", "2021-02-02", "2022-01-03", "2022-01-04", "2022-05-05"))
#' differences <- c(100, -50, 200, -150, -300)
#' calculate_financial_transfer_vat(dates, differences)
#' @export
calculate_financial_transfer_vat <- function(dates, differences,vat_rate=0.06) {
  if (length(dates) != length(differences)) {
    stop("The lengths of 'dates' and 'differences' should be the same.")
  }
  dates <- as.Date(dates)
  year_month <- format(dates, "%Y-%m")

  sales_amount <- 0
  carry_forward_loss <- 0

  result_df <- data.frame(
    Year_Month = character(),
    Sales_Amount = numeric(),
    VAT = numeric(),
    stringsAsFactors = FALSE
  )

  unique_year_months <- unique(year_month)
  for (ym in unique_year_months) {

    ym_dates <- dates[year_month == ym]
    ym_differences <- differences[year_month == ym]

    current_difference <- sum(ym_differences)

    if(carry_forward_loss < 0) {
      current_difference <- current_difference + carry_forward_loss
      carry_forward_loss <- 0
    }

    if(current_difference > 0) {
      sales_amount <- current_difference
    }else{
      sales_amount <- 0
      carry_forward_loss <- current_difference
    }

    vat <- sales_amount/(1+vat_rate) * vat_rate

    new_row <- data.frame(
      Year_Month = ym,
      Sales_Amount = sales_amount,
      VAT = vat
    )
    result_df <- rbind(result_df, new_row)
  }

  return(result_df)
}
