module Utils.Error.Buckets where
 
import ECPrelude
 
import Config.Constants (ecRedis)
import Engineering.Types.API (ECErrorPayload(..), ErrorPayload(..), EulerErrorType(..), EulerErrorPayload(..), ECOrderStatusErrorPayload(..), ECTxnStatusErrorPayload(..))
import Euler.Utils.Utils (getXRequestId)
import Presto.Backend.Flow (BackendFlow)
import Presto.Backend.Flow (doAffRR')
import Presto.Backend.Flow (ask) as F
import Euler.Utils.Utils (getXRequestId)
import Engineering.Types.API (EulerErrorType(..)) as EulerErrorType
import Types.Lenses (_errorMessage, _error_code, _userMessage, _error_message, _status)
 
nullOrUndefinedToStr :: NullOrUndefined String -> String
nullOrUndefinedToStr val = fromMaybe "" $ unNullOrUndefined val
 
ecErrorToEulerErrorPayload :: forall rt st d. ECErrorPayload -> BackendFlow st rt EulerErrorPayload
ecErrorToEulerErrorPayload inputPayload = do
 let bucket = ecerrorbucket (inputPayload ^. _status) (nullOrUndefinedToStr (inputPayload ^. _error_message))
 sId <- doAffRR' "ecErrorToEulerErrorPayload" (liftEff getXRequestId)
 pure $ EulerErrorPayload
         { "type": bucket."type"
         , code : nullOrUndefinedToStr (inputPayload ^. _error_code)
         , message : bucket.msg
         , user_message : NullOrUndefined (Just bucket.usrMsg)
         , session_id : NullOrUndefined (Just sId)
         , faq : bucket.faq
         }

ecerrorbucket :: String -> String -> { "type" :: EulerErrorType, code :: String, usrMsg :: String, msg :: String, faq :: String }
ecerrorbucket status errmsg | (status == "Invalid_Request_error") && (errmsg == "Mandate Not Found") = {"type" :EulerErrorType.INVALID_REQUEST_ERROR, code : "Invalid_Request", usrMsg : "The user has entered bank details incorrectly", msg : "Mandate Not Found", faq : "/error/faq#mandate_not_found" }
                            | (status == "Invalid_Request_error") && (errmsg == "Transaction is inprocess") = {"type" :EulerErrorType.INVALID_REQUEST_ERROR, code : "Invalid_Request", usrMsg : "Transaction is inprocess", msg : "Transaction is inprocess", faq : "/error/faq#transaction_is_inprocess" }
                            | (status == "Invalid_Request_error") && (errmsg == "Transaction limit reached for order") = {"type" :EulerErrorType.INVALID_REQUEST_ERROR, code : "Invalid_Request", usrMsg : "Transaction limit reached for order", msg : "Transaction limit reached for order", faq : "/error/faq#transaction_limit_reached_for_order" }
                            | (status == "Invalid_Request_error") && (errmsg == "merchant account cannot be null") = {"type" :EulerErrorType.INVALID_REQUEST_ERROR, code : " Invalid_Request ", usrMsg : " Please check if merchant account is not null ", msg : "merchant account cannot be null", faq : "/error/faq#merchant_account_cannot_be_null" }
                            | (status == "Invalid_Request_error") && (errmsg == "Invalid merchant_id") = {"type" :EulerErrorType.INVALID_REQUEST_ERROR, code : " Invalid_Request ", usrMsg : " Merchant id is invalid ", msg : "Invalid merchant_id", faq : "/error/faq#invalid_merchant_id" }
                            | (status == "Invalid_Request_error") && (errmsg == "No card could be located for token") = {"type" :EulerErrorType.INVALID_REQUEST_ERROR, code : " Invalid_Request ", usrMsg : " Please check if card exists for given token ", msg : "No card could be located for token", faq : "/error/faq#no_card_could_be_located_for_token" }
                            | (status == "Invalid_Request_error") && (errmsg == "txnCardInfo missing for txnid") = {"type" :EulerErrorType.INVALID_REQUEST_ERROR, code : " Invalid_Request ", usrMsg : " Please check transaction Id ", msg : "txnCardInfo missing for txnid", faq : "/error/faq#txncardinfo_missing_for_txnid" }
                            | (status == "Invalid_Request_error") && (errmsg == "Can't find a suitable gateway to process the transaction") = {"type" :EulerErrorType.INVALID_REQUEST_ERROR, code : " Invalid_Request ", usrMsg : " Unable to find suitable gateway please try other payment options ", msg : "Can't find a suitable gateway to process the transaction", faq : "/error/faq#cannot_find_a_suitable_gateway_to_process_the_transaction" }
                            | (status == "Invalid_Request_error") && (errmsg == "second factor not found for this Txnid") = {"type" :EulerErrorType.INVALID_REQUEST_ERROR, code : " Invalid_Request ", usrMsg : " Login issue : second factor not found for this Txnid ", msg : "second factor not found for this Txnid", faq : "/error/faq#second_factor_not_found_for_this_txnid" }
                            | (status == "Invalid_Request_error") && (errmsg == "Payment method not found") = {"type" :EulerErrorType.INVALID_REQUEST_ERROR, code : " Invalid_Request ", usrMsg : " Payment method not found please choose another payment option ", msg : "Payment method not found", faq : "/error/faq#payment_method_not_found" }
                            | (status == "Invalid_Request_error") && (errmsg == "txnOffer missing for txnid") = {"type" :EulerErrorType.INVALID_REQUEST_ERROR, code : " Invalid_Request ", usrMsg : " TxnOffer not available for this txnId ", msg : "txnOffer missing for txnid", faq : "/error/faq#txnoffer_missing_for_txnid" }
                            | (status == "Invalid_Request_error") && (errmsg == "this entity is already deleted") = {"type" :EulerErrorType.INVALID_REQUEST_ERROR, code : " Invalid_Request ", usrMsg : " This entity is already deleted from bank accounts ", msg : "this entity is already deleted", faq : "/error/faq#this_entity_is_already_deleted" }
                            | (status == "bad_origin") && (errmsg == "Bad Origin. Permission denied.") = {"type" :EulerErrorType.AUTHENTICATION_ERROR, code : " Bad_origin ", usrMsg : " IP validation failed -Bad Origin, Permission denied ", msg : "Bad_Origin_Permission_denied", faq : "/error/faq#bad_origin_permission_denied" }
                            | (status == "Forbidden_Resource") && (errmsg == "Authentication is required for fetching order details") = {"type" :EulerErrorType.AUTHENTICATION_ERROR, code : " Forbidden_Resource ", usrMsg : " Authentication required for fetching details ", msg : "Authentication required for fetching details", faq : "/error/faq#authentication_is_required_for_fetching_order_details" }
                            | (status == "Access_denied") && (errmsg == "") = {"type" :EulerErrorType.AUTHENTICATION_ERROR, code : " Authentication error ", usrMsg : " access_denied ", msg : " access_denied ", faq : "/error/faq#access_denied" }
                            | (status == "Access_denied") && (errmsg == "Merchant_Id_info_missing") = {"type" :EulerErrorType.AUTHENTICATION_ERROR, code : " Authentication error ", usrMsg : " your merchant_Id info not found ", msg : "Merchant_Id_info_missing", faq : "/error/faq#merchant_id_info_missing" }
                            | (status == "Error") && (errmsg == "Mandatory fields are missing") = {"type" :EulerErrorType.INVALID_REQUEST, code : "Invalid Request", usrMsg : " You are missing mandatory fileds ", msg : "Mandatory fields are missing", faq : "/error/faq#mandatory_fields_are_missing" }
                            | (status == "Error") && (errmsg == "Provided command not found for given orderId") = {"type" :EulerErrorType.INTERNAL_SERVER_ERROR, code : " CommandNotFound ", usrMsg : " Provided command not found for given orderId ", msg : "Provided command not found for given orderId", faq : "/error/faq#provided_command_not_found_for_given_orderid" }
                            | (status == "Error") && (errmsg == "Internal server error") = {"type" :EulerErrorType.INTERNAL_SERVER_ERROR, code : " Internal server error ", usrMsg : " Internal server error ", msg : "Internal server error", faq : "/error/faq#internal_server_error-1" }
                            | (status == "Error") && (errmsg == "Access Denied") = {"type" :EulerErrorType.AUTHENTICATION_ERROR, code : " ecAccessDenied ", usrMsg : "", msg : "Access Denied", faq : "/error/faq#access_denied" }
                            | (status == "ERROR") && (errmsg == "No successfull txn found for given order_id") = {"type" :EulerErrorType.INVALID_REQUEST, code : " No txn found ", usrMsg : "No successfull txn found for given order_id", msg : "No successfull txn found for given order_id", faq : "/error/faq#no_successfull_txn_found_for_given_order_id" }
                            | (status == "ERROR") && (errmsg == "VPA verification not supported for the given merchant Id") = {"type" :EulerErrorType.ERROR, code : "Error", usrMsg : " processing error ", msg : " vpa ferification not supported for given merchant Id ", faq : "/error/faq#vpa_verification_not_supported_for_the_given_merchant_id"}
                            | (status == "DUPLICATE_ORDER_ID") && (errmsg == "Order already exists with the given order_id") = {"type" :EulerErrorType.INVALID_REQUEST, code : "Duplicate order Id", usrMsg : "Order already exists with the given order_id", msg : "Order already exists with the given order_id", faq : "/error/faq#order_already_exists_with_the_given_order_id" }
                            | (status == "DUPLICATE_REQUEST") && (errmsg == "Duplicate Request") = {"type" :EulerErrorType.INVALID_REQUEST, code : " Duplicate request ", usrMsg : " Duplicate Request", msg : "Duplicate Request", faq : "/error/faq#duplicate_request" }
                            | (status == "NOT_FOUND") && (errmsg == "Customer not found for this order") = {"type" :EulerErrorType.INVALID_REQUEST, code : " Customer Not Found ", usrMsg : " Customer not found for this order ", msg : "Customer not found for this order", faq : "/error/faq#customer_not_found_for_this_order" }
                            | (status == "Not_found") && (errmsg == "") = {"type" :EulerErrorType.AUTHENTICATION_ERROR, code : " Not Found ", usrMsg : " Not Found ", msg : " Not Found ", faq : "/error/faq#authentication_error" }
                            | (status == "Not_found") && (errmsg == "Refund Details Not found the request") = {"type" :EulerErrorType.INVALID_REQUEST, code : " Not Found ", usrMsg : " Refund details not found for the request ", msg : "Refund Details Not found the request", faq : "/error/faq#refund_details_not_found_the_request" }
                            | (status == "Not_found") && (errmsg == "Gateway Not supported") = {"type" :EulerErrorType.UPSTREAM_API_ERROR, code : " Not_Found ", usrMsg : " This Gateway is Not supported ", msg : "Gateway Not supported", faq : "/error/faq#gateway_not_supported" }
                            | (status == "Not_found") && (errmsg == "Authentication Account not found") = {"type" :EulerErrorType.INTERNAL_SERVER_ERROR, code : " Not_found ", usrMsg : " Authentication account not found ", msg : "Authentication Account not found", faq : "/error/faq#authentication_account_not_found" }
                            | (status == "session_expired") && (errmsg == "Session Expired") = {"type" :EulerErrorType.UPSTREAM_API_ERROR, code : "sessionTimeout", usrMsg : " Your session expired ", msg : "Session Expired", faq : "/error/faq#session_expired" }
                            | (status == "network_not_supported") && (errmsg == "Network Not Supported") = {"type" :EulerErrorType.INTERNAL_SERVER_ERROR, code : "Network Not Supported", usrMsg : "network not supported", msg : "Network Not Supported", faq : "/error/faq#network_not_supported" }
                            | (status == "payment_type_not_supported") && (errmsg == "Payment type Not supported") = {"type" :EulerErrorType.INTERNAL_SERVER_ERROR, code : "Payment type not supported", usrMsg : "payment type not supported", msg : "Payment type Not supported", faq : "/error/faq#payment_type_not_supported" }
                            | (status == "Not_found") && (errmsg == "") = {"type" :EulerErrorType.INTERNAL_SERVER_ERROR, code : " Not_found ", usrMsg : " INTERNAL SERVER ERROR", msg : "Internal server error", faq : "/error/faq#internal_server_error" }
                            | (status == "Not_found") && (errmsg == "") = {"type" :EulerErrorType.UPSTREAM_API_ERROR, code : " Not_found ", usrMsg : "UPSTREAM API ERROR", msg : "Upstream Api Error", faq : "/error/faq#upstream_api_error" }
                            | (status == "internal_server_error") && (errmsg == "Encryption failed") = {"type" :EulerErrorType.INTERNAL_SERVER_ERROR, code : " Internal Server Error ", usrMsg : " Encryption failed ", msg : "Encryption failed", faq : "/error/faq#encryption_failed" }
                            | (status == "internal_server_error") && (errmsg == "Decryption failed") = {"type" :EulerErrorType.INTERNAL_SERVER_ERROR, code : " Internal Server Error ", usrMsg : " Decryption failed ", msg : "Decryption failed", faq : "/error/faq#decryption_failed" }
                            | (status == "UNPROCESSABLE_ENTITY") && (errmsg == "unprocessable entity") = {"type" :EulerErrorType.INTERNAL_SERVER_ERROR, code : "Unprocessable entity", usrMsg : "unprocessable entity", msg : "unprocessable entity", faq : "/error/faq#unprocessable_entity"}
                            | otherwise = {"type" :EulerErrorType.ERROR, code : "Error", usrMsg : "Processing error", msg : "Processing error", faq : "/error/faq#processing_error"}

errorPayloadToEulerErrorPayload :: forall a rt. ErrorPayload -> EulerErrorType -> String -> String -> BackendFlow rt {sessionId :: String | a} EulerErrorPayload
errorPayloadToEulerErrorPayload inputPayload errorType ercode fraq = do
 config <- F.ask
 pure $ EulerErrorPayload
         {
         "type" : errorType
         , code : ercode
         , message : (inputPayload ^. _errorMessage)
         , user_message : NullOrUndefined (Just (inputPayload ^. _userMessage))
         , session_id : NullOrUndefined (Just config.sessionId)
         , faq : fraq
         }
 
ecTxnStatusPayloadToEulerErrorPayload :: forall a rt. ECTxnStatusErrorPayload -> EulerErrorType -> String -> BackendFlow rt {sessionId :: String | a} EulerErrorPayload
ecTxnStatusPayloadToEulerErrorPayload inputPayload errorType fraq = do
 config <- F.ask
 pure $ EulerErrorPayload
         {
         "type" : errorType
         , code : nullOrUndefinedToStr (inputPayload ^. _error_code)
         , message : nullOrUndefinedToStr (inputPayload ^. _error_message)
         , user_message : (inputPayload ^. _error_message)
         , session_id : NullOrUndefined (Just config.sessionId)
         , faq : fraq
         }
 
ecOrderStatusPayloadToEulerErrorPayload :: forall a rt. ECOrderStatusErrorPayload -> EulerErrorType -> String -> BackendFlow rt {sessionId :: String | a} EulerErrorPayload
ecOrderStatusPayloadToEulerErrorPayload inputPayload errorType fraq = do
 config <- F.ask
 pure $ EulerErrorPayload
         {
         "type" : errorType
         , code : nullOrUndefinedToStr (inputPayload ^. _error_code)
         , message : nullOrUndefinedToStr (inputPayload ^. _error_message)
         , user_message : (inputPayload ^. _error_message)
         , session_id : NullOrUndefined (Just config.sessionId)
         , faq : fraq
         }

