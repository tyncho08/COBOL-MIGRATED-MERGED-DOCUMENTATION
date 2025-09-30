'use client'

import { useState, useEffect } from 'react'
import { 
  CurrencyDollarIcon,
  CalendarDaysIcon,
  ExclamationTriangleIcon,
  ClockIcon,
  DocumentTextIcon,
  EnvelopeIcon
} from '@heroicons/react/24/outline'
import { Card, StatsCard } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import Table from '@/components/UI/Table'
import PageHeader from '@/components/Layout/PageHeader'
import { formatCurrency, formatDate } from '@/lib/utils'
import { EmailService } from '@/lib/emailService'

interface OutstandingInvoice {
  id: number
  invoice_number: string
  customer_code: string
  customer_name: string
  invoice_date: string
  due_date: string
  amount: number
  balance: number
  days_overdue: number
  status: 'current' | 'overdue' | 'critical'
}

interface OutstandingSummary {
  total_outstanding: number
  current_amount: number
  overdue_amount: number
  critical_amount: number
  total_invoices: number
  overdue_invoices: number
  average_days_overdue: number
  oldest_invoice_days: number
}

export default function OutstandingPage() {
  const [summary, setSummary] = useState<OutstandingSummary | null>(null)
  const [invoices, setInvoices] = useState<OutstandingInvoice[]>([])
  const [loading, setLoading] = useState(true)
  const [selectedInvoices, setSelectedInvoices] = useState<Set<number>>(new Set())

  useEffect(() => {
    fetchOutstandingData()
  }, [])

  const fetchOutstandingData = async () => {
    try {
      setLoading(true)
      
      // Fetch outstanding invoices from COBOL endpoint
      const response = await fetch('http://localhost:8000/api/v1/sales/cobol/outstanding')
      if (response.ok) {
        const data = await response.json()
        setSummary(data.summary)
        setInvoices(data.invoices || [])
      } else {
        console.error('Failed to fetch outstanding data:', response.status)
      }
    } catch (error) {
      console.error('Failed to fetch outstanding data:', error)
    } finally {
      setLoading(false)
    }
  }

  const handleStatementEmail = () => {
    const selectedIds = Array.from(selectedInvoices)
    if (selectedIds.length === 0) {
      alert('Please select invoices to include in the statement')
      return
    }
    
    // Get selected invoice data and group by customer
    const selectedInvoiceData = invoices.filter(inv => selectedIds.includes(inv.id))
    const customerGroups = selectedInvoiceData.reduce((groups: any, inv) => {
      if (!groups[inv.customer_code]) {
        groups[inv.customer_code] = {
          customer_name: inv.customer_name,
          invoices: []
        }
      }
      groups[inv.customer_code].invoices.push(inv)
      return groups
    }, {})
    
    // Send statement for each customer
    Object.entries(customerGroups).forEach(([customerCode, data]: any) => {
      const customerEmail = prompt(`Enter email address for ${data.customer_name}:`, 'customer@example.com')
      
      if (customerEmail) {
        const statementData = {
          totalOutstanding: data.invoices.reduce((sum: number, inv: any) => sum + inv.balance, 0),
          current: data.invoices.filter((inv: any) => inv.status === 'current').reduce((sum: number, inv: any) => sum + inv.balance, 0),
          overdue: data.invoices.filter((inv: any) => inv.status !== 'current').reduce((sum: number, inv: any) => sum + inv.balance, 0),
          days30: data.invoices.filter((inv: any) => inv.days_overdue > 0 && inv.days_overdue <= 30).reduce((sum: number, inv: any) => sum + inv.balance, 0),
          days60: data.invoices.filter((inv: any) => inv.days_overdue > 30 && inv.days_overdue <= 60).reduce((sum: number, inv: any) => sum + inv.balance, 0),
          days90: data.invoices.filter((inv: any) => inv.days_overdue > 60 && inv.days_overdue <= 90).reduce((sum: number, inv: any) => sum + inv.balance, 0),
          daysOver90: data.invoices.filter((inv: any) => inv.days_overdue > 90).reduce((sum: number, inv: any) => sum + inv.balance, 0)
        }
        
        EmailService.sendStatement(
          { customer_code: customerCode, customer_name: data.customer_name, email: customerEmail },
          statementData
        )
      }
    })
  }

  const handleChasePayment = () => {
    const selectedIds = Array.from(selectedInvoices)
    if (selectedIds.length === 0) {
      alert('Please select invoices to chase')
      return
    }
    
    // Get selected invoice data
    const selectedInvoiceData = invoices.filter(inv => selectedIds.includes(inv.id))
    
    selectedInvoiceData.forEach((invoice) => {
      const customerEmail = prompt(`Enter email address for ${invoice.customer_name}:`, 'customer@example.com')
      
      if (customerEmail) {
        EmailService.chasePayment({
          ...invoice,
          invoice_number: invoice.invoice_number,
          invoice_due_date: invoice.due_date,
          invoice_total_amount: invoice.amount,
          invoice_balance: invoice.balance
        }, customerEmail)
      }
    })
  }

  const getStatusBadge = (status: string) => {
    switch (status) {
      case 'current':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
            Current
          </span>
        )
      case 'overdue':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-yellow-100 text-yellow-800">
            Overdue
          </span>
        )
      case 'critical':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
            Critical
          </span>
        )
      default:
        return null
    }
  }

  const columns = [
    {
      key: 'invoice_number',
      header: 'Invoice',
      render: (value: any, row: OutstandingInvoice) => (
        <div>
          <div className="font-medium text-gray-900">{row.invoice_number}</div>
          <div className="text-sm text-gray-500">{row.customer_name}</div>
        </div>
      )
    },
    {
      key: 'invoice_date',
      header: 'Invoice Date',
      render: (value: any, row: OutstandingInvoice) => formatDate(row.invoice_date)
    },
    {
      key: 'due_date',
      header: 'Due Date',
      render: (value: any, row: OutstandingInvoice) => (
        <div>
          <div>{formatDate(row.due_date)}</div>
          {row.days_overdue > 0 && (
            <div className="text-sm text-red-600">{row.days_overdue} days overdue</div>
          )}
        </div>
      )
    },
    {
      key: 'amount',
      header: 'Original',
      render: (value: any, row: OutstandingInvoice) => formatCurrency(row.amount)
    },
    {
      key: 'balance',
      header: 'Outstanding',
      render: (value: any, row: OutstandingInvoice) => (
        <div className="font-medium text-gray-900">{formatCurrency(row.balance)}</div>
      )
    },
    {
      key: 'status',
      header: 'Status',
      render: (value: any, row: OutstandingInvoice) => getStatusBadge(row.status)
    }
  ]

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm" onClick={handleStatementEmail}>
        <EnvelopeIcon className="h-4 w-4" />
        Send Statements
      </Button>
      <Button variant="outline" size="sm" onClick={handleChasePayment}>
        <ExclamationTriangleIcon className="h-4 w-4" />
        Chase Payment
      </Button>
      <Button size="sm">
        <DocumentTextIcon className="h-4 w-4" />
        Aging Report
      </Button>
    </div>
  )

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Outstanding Invoices"
        description="Manage unpaid customer invoices and overdue accounts"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Sales', href: '/sales' },
          { label: 'Outstanding' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Summary Stats */}
        {summary && (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
            <StatsCard
              title="Total Outstanding"
              value={formatCurrency(summary.total_outstanding)}
              icon={<CurrencyDollarIcon className="h-6 w-6" />}
              change={{ 
                value: `${summary.total_invoices} invoices`, 
                type: 'neutral' 
              }}
            />
            <StatsCard
              title="Current"
              value={formatCurrency(summary.current_amount)}
              icon={<ClockIcon className="h-6 w-6" />}
              className="text-green-600"
              change={{ 
                value: `Within terms`, 
                type: 'increase' 
              }}
            />
            <StatsCard
              title="Overdue"
              value={formatCurrency(summary.overdue_amount)}
              icon={<CalendarDaysIcon className="h-6 w-6" />}
              className="text-yellow-600"
              change={{ 
                value: `${summary.overdue_invoices} invoices`, 
                type: 'decrease' 
              }}
            />
            <StatsCard
              title="Critical (>60 days)"
              value={formatCurrency(summary.critical_amount)}
              icon={<ExclamationTriangleIcon className="h-6 w-6" />}
              className="text-red-600"
              change={{ 
                value: `Oldest: ${summary.oldest_invoice_days} days`, 
                type: 'decrease' 
              }}
            />
          </div>
        )}

        {/* Outstanding Invoices Table */}
        <Card>
          <div className="px-6 py-4 border-b border-gray-200">
            <h3 className="text-lg font-medium text-gray-900">Outstanding Invoices</h3>
          </div>
          <Table
            data={invoices}
            columns={columns}
            loading={loading}
            emptyMessage="No outstanding invoices"
            selection={{
              selectedRows: selectedInvoices,
              onRowSelect: (index: number) => {
                const newSelected = new Set(selectedInvoices)
                const invoice = invoices[index]
                if (newSelected.has(invoice.id)) {
                  newSelected.delete(invoice.id)
                } else {
                  newSelected.add(invoice.id)
                }
                setSelectedInvoices(newSelected)
              },
              onSelectAll: () => {
                if (selectedInvoices.size === invoices.length) {
                  setSelectedInvoices(new Set())
                } else {
                  setSelectedInvoices(new Set(invoices.map(inv => inv.id)))
                }
              }
            }}
          />
        </Card>
      </main>
    </div>
  )
}