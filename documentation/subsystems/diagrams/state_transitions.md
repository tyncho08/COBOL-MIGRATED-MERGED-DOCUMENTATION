# ACAS State Transition Diagrams

## Overview

This document captures the state transitions for key business entities and processes within the ACAS system. Understanding these state machines is crucial for maintaining data integrity and business rule compliance during migration.

## Customer Order State Transitions

```mermaid
stateDiagram-v2
    [*] --> Draft: Create Order
    
    Draft --> Validated: Validate
    Draft --> Cancelled: Cancel
    
    Validated --> Confirmed: Credit Approved
    Validated --> CreditHold: Credit Failed
    Validated --> Cancelled: Cancel
    
    CreditHold --> Confirmed: Override
    CreditHold --> Cancelled: Reject
    
    Confirmed --> Allocated: Stock Available
    Confirmed --> Backorder: No Stock
    Confirmed --> PartialAlloc: Partial Stock
    
    Allocated --> Picked: Pick List
    Backorder --> Allocated: Stock Received
    PartialAlloc --> Picked: Pick Available
    PartialAlloc --> Allocated: Stock Received
    
    Picked --> Shipped: Ship Confirm
    Picked --> Cancelled: Cancel Shipment
    
    Shipped --> Invoiced: Generate Invoice
    Shipped --> Returned: Return Initiated
    
    Invoiced --> PartialPaid: Partial Payment
    Invoiced --> Paid: Full Payment
    Invoiced --> Overdue: Payment Late
    
    PartialPaid --> Paid: Final Payment
    PartialPaid --> Overdue: Payment Late
    
    Overdue --> PartialPaid: Payment Received
    Overdue --> Paid: Payment Received
    Overdue --> WriteOff: Bad Debt
    
    Paid --> Closed: Period Close
    WriteOff --> Closed: Period Close
    Returned --> Credited: Credit Issued
    Credited --> Closed: Complete
    
    Cancelled --> [*]: End
    Closed --> [*]: End
```

### Order State Business Rules

| State | Allowed Actions | Restrictions |
|-------|----------------|--------------|
| Draft | Edit all fields, delete | No GL impact |
| Validated | Edit quantity, cancel | Credit check required |
| Confirmed | Cancel with approval | Inventory allocated |
| Shipped | Return only | Cannot cancel |
| Invoiced | Apply payments | Cannot edit |
| Paid | View only | Locked |

## Purchase Order State Transitions

```mermaid
stateDiagram-v2
    [*] --> Requisition: Create Request
    
    Requisition --> Approved: Approve
    Requisition --> Rejected: Reject
    
    Approved --> PO_Created: Convert to PO
    Rejected --> Requisition: Revise
    Rejected --> [*]: Abandon
    
    PO_Created --> Sent: Send to Vendor
    PO_Created --> Cancelled: Cancel
    
    Sent --> Acknowledged: Vendor Confirms
    Sent --> Cancelled: Cancel
    
    Acknowledged --> PartialReceived: Partial Delivery
    Acknowledged --> FullyReceived: Complete Delivery
    Acknowledged --> Cancelled: Cancel
    
    PartialReceived --> FullyReceived: Final Delivery
    PartialReceived --> Cancelled: Cancel Balance
    
    FullyReceived --> Invoiced: Invoice Received
    FullyReceived --> Completed: No Invoice
    
    Invoiced --> Matched: 3-Way Match
    Invoiced --> Disputed: Match Failed
    
    Matched --> Approved_Payment: Approve
    Disputed --> Matched: Resolve
    Disputed --> Cancelled: Reject
    
    Approved_Payment --> Paid: Payment Run
    Paid --> Completed: Close PO
    
    Completed --> [*]: End
    Cancelled --> [*]: End
```

## Invoice Lifecycle States

```mermaid
stateDiagram-v2
    [*] --> Generated: Create Invoice
    
    Generated --> Printed: Print
    Generated --> Electronic: Send EDI
    Generated --> Cancelled: Cancel
    
    Printed --> Sent: Mail
    Electronic --> Sent: Transmitted
    
    Sent --> Outstanding: Awaiting Payment
    Sent --> Disputed: Customer Dispute
    
    Outstanding --> PartialPaid: Partial Payment
    Outstanding --> FullPaid: Full Payment
    Outstanding --> Overdue: Past Due
    
    Disputed --> Adjusted: Credit/Debit Note
    Disputed --> Outstanding: Resolved
    
    Adjusted --> Outstanding: New Balance
    Adjusted --> FullPaid: Settled
    
    PartialPaid --> Outstanding: Balance Due
    PartialPaid --> FullPaid: Final Payment
    
    Overdue --> Collection: Send to Collections
    Overdue --> PartialPaid: Payment Received
    Overdue --> FullPaid: Payment Received
    
    Collection --> PartialPaid: Payment Plan
    Collection --> FullPaid: Settled
    Collection --> WrittenOff: Bad Debt
    
    FullPaid --> Closed: Period End
    WrittenOff --> Closed: Period End
    
    Cancelled --> [*]: Void
    Closed --> [*]: Archived
```

## Inventory Item States

```mermaid
stateDiagram-v2
    [*] --> Available: Stock Receipt
    
    Available --> Allocated: Order Allocation
    Available --> Reserved: Manual Reserve
    Available --> Damaged: Quality Issue
    Available --> Expired: Shelf Life
    
    Allocated --> Picked: Pick Confirmed
    Allocated --> Available: Order Cancelled
    
    Reserved --> Available: Release
    Reserved --> Allocated: Convert to Order
    
    Picked --> Shipped: Ship Confirmed
    Picked --> Available: Pick Cancelled
    
    Shipped --> Sold: Invoice Posted
    Shipped --> Returned: Customer Return
    
    Returned --> Available: Good Condition
    Returned --> Damaged: Bad Condition
    
    Damaged --> Scrapped: Write Off
    Damaged --> Available: Repaired
    
    Expired --> Scrapped: Dispose
    
    Sold --> [*]: Removed from Stock
    Scrapped --> [*]: Removed from Stock
```

## GL Period States

```mermaid
stateDiagram-v2
    [*] --> Future: Period Created
    
    Future --> Open: Period Start
    
    Open --> Closing: Begin Close
    Open --> Locked: Emergency Lock
    
    Closing --> Adjusting: Adjustments Needed
    Closing --> Closed: Close Complete
    
    Adjusting --> Closing: Adjustments Done
    
    Locked --> Open: Unlock
    Locked --> Closed: Force Close
    
    Closed --> Archived: Year End
    Closed --> Reopened: Correction Needed
    
    Reopened --> Open: Make Corrections
    
    Archived --> [*]: Historical
```

### Period State Rules

| State | Posting Allowed | Actions Available |
|-------|----------------|-------------------|
| Future | No | Open when current |
| Open | Yes | Post, close, lock |
| Closing | Limited | Adjustments only |
| Closed | No | Reopen with approval |
| Archived | No | Read only |

## User Session States

```mermaid
stateDiagram-v2
    [*] --> Login: Start Session
    
    Login --> Authenticating: Submit Credentials
    
    Authenticating --> Active: Success
    Authenticating --> Failed: Invalid
    
    Failed --> Login: Retry
    Failed --> Locked: Max Attempts
    
    Active --> Idle: No Activity
    Active --> Working: User Action
    
    Working --> Active: Complete
    Working --> Timeout_Warning: Long Operation
    
    Idle --> Timeout_Warning: Near Timeout
    
    Timeout_Warning --> Active: User Action
    Timeout_Warning --> Timed_Out: No Response
    
    Active --> Logged_Out: User Logout
    Timed_Out --> Logged_Out: Auto Logout
    
    Locked --> [*]: Admin Required
    Logged_Out --> [*]: End Session
```

## Batch Job States

```mermaid
stateDiagram-v2
    [*] --> Scheduled: Job Created
    
    Scheduled --> Waiting: Dependencies
    Scheduled --> Ready: No Dependencies
    
    Waiting --> Ready: Dependencies Met
    Waiting --> Cancelled: Timeout
    
    Ready --> Running: Start Execution
    Ready --> Cancelled: Manual Cancel
    
    Running --> Completed: Success
    Running --> Failed: Error
    Running --> Warning: Partial Success
    Running --> Aborted: System Stop
    
    Failed --> Ready: Retry
    Failed --> Cancelled: Max Retries
    
    Warning --> Completed: Accept
    Warning --> Failed: Reject
    
    Aborted --> Ready: Restart
    
    Completed --> Archived: Log Cleanup
    Cancelled --> Archived: Log Cleanup
    
    Archived --> [*]: Historical
```

## Payment States

```mermaid
stateDiagram-v2
    [*] --> Selected: Payment Selection
    
    Selected --> Approved: Authorization
    Selected --> Rejected: Denied
    
    Approved --> Processing: Payment Run
    Rejected --> [*]: End
    
    Processing --> Sent: Bank Transfer
    Processing --> Printed: Check Print
    Processing --> Failed: Error
    
    Sent --> Pending: Awaiting Clear
    Printed --> Pending: Check Issued
    Failed --> Selected: Retry
    
    Pending --> Cleared: Bank Confirmed
    Pending --> Returned: NSF/Rejected
    Pending --> Cancelled: Stop Payment
    
    Cleared --> Reconciled: Bank Rec
    Returned --> Selected: Reprocess
    Cancelled --> Voided: Record Void
    
    Reconciled --> [*]: Complete
    Voided --> [*]: Complete
```

## Document Approval Workflow States

```mermaid
stateDiagram-v2
    [*] --> Submitted: Create Document
    
    Submitted --> Level1_Review: Route
    
    Level1_Review --> Level1_Approved: Approve
    Level1_Review --> Rejected: Reject
    Level1_Review --> Clarification: Query
    
    Clarification --> Level1_Review: Response
    
    Level1_Approved --> Level2_Review: Above Threshold
    Level1_Approved --> Final_Approved: Within Limit
    
    Level2_Review --> Level2_Approved: Approve
    Level2_Review --> Rejected: Reject
    Level2_Review --> Level1_Review: Send Back
    
    Level2_Approved --> Level3_Review: Above Threshold
    Level2_Approved --> Final_Approved: Within Limit
    
    Level3_Review --> Final_Approved: Approve
    Level3_Review --> Rejected: Reject
    
    Rejected --> Submitted: Revise
    Rejected --> Cancelled: Abandon
    
    Final_Approved --> Processed: Execute
    
    Processed --> [*]: Complete
    Cancelled --> [*]: End
```

## Master Data Record States

```mermaid
stateDiagram-v2
    [*] --> Draft: Create New
    
    Draft --> Pending_Approval: Submit
    Draft --> Cancelled: Abandon
    
    Pending_Approval --> Active: Approved
    Pending_Approval --> Draft: Rejected
    
    Active --> On_Hold: Temporary Block
    Active --> Inactive: Permanent Block
    Active --> Pending_Change: Modification
    
    On_Hold --> Active: Release
    On_Hold --> Inactive: Permanent
    
    Pending_Change --> Active: Approved
    Pending_Change --> Active: Rejected
    
    Inactive --> Archived: Time Limit
    Inactive --> Active: Reactivate
    
    Archived --> [*]: Historical
    Cancelled --> [*]: Deleted
```

## Transaction Processing States

```mermaid
stateDiagram-v2
    [*] --> Initiated: Start Transaction
    
    Initiated --> Validating: Submit
    
    Validating --> Valid: Pass
    Validating --> Invalid: Fail
    
    Invalid --> Initiated: Correct
    Invalid --> Aborted: Cancel
    
    Valid --> Processing: Execute
    
    Processing --> Completed: Success
    Processing --> Partial: Partial Success
    Processing --> Failed: Error
    
    Partial --> Completed: Resolve
    Partial --> Failed: Abandon
    
    Failed --> Initiated: Retry
    Failed --> Aborted: Give Up
    
    Completed --> Posted: Update GL
    
    Posted --> [*]: Done
    Aborted --> [*]: Cancelled
```

## State Transition Audit Requirements

### Audit Information Captured

For each state transition:
- Previous state
- New state  
- Timestamp
- User/System ID
- Reason code
- Authorization (if required)
- Related transaction ID

### State History Retention

| Entity Type | Retention Period | Archive Method |
|-------------|-----------------|----------------|
| Financial Transactions | 7 years | Compressed archive |
| Master Data | Lifetime + 3 years | Full history |
| User Sessions | 90 days | Rolling deletion |
| Batch Jobs | 1 year | Summary only |
| System States | 30 days | Log rotation |